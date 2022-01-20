/* Module representing a generic data model */
module type Model = {
  /* [t] is the type representing the data model */
  type t

  /* [empty] is the empty data model */
  let empty: t
}

module type S = {
  /* [model] is the data model produced after decoding is complete */
  type model

  /* [value] is the type of value decoded by the decoder */
  type value

  /* [type t] is the type of decoder [t] represented by type ['a] */
  type t<'a>

  /* [toBool v] is the boolean value returned by decoding [v] */
  let toBool: value => bool

  /* [parse s] is the decoder produced by parsing [s].
   Raises: (failure) if [s] is unable to be parsed */
  let parse: string => t<'a>

  /* [decode t fn] is the model produced by reading the values from the decoder into
   the data [model] */
  let decode: (t<'a>, t<'a> => model) => t<'a>

  /* [decodeNum t s f] is the decoder produced by decoding number [s] in [t] and
   by application of [f] on the model and a [float]. */
  let decodeNum: (t<'a>, string) => float

  /* [decodeBool t s f] is the decoder produced by decoding boolean [s] in [t]
   and by application of [f] on the model and a [bool]. */
  let decodeBool: (t<'a>, string) => bool

  /* [decodeStr t s f] is the decoder produced by decoding string [s] in [t].
   and by application of [f] on the model and a [string]. */
  let decodeStr: (t<'a>, string) => string

  /* [decodeObj t s f] is the decoder produced by decoding object [s] in [t].
     Attributes of the object must be decoded using the other primitive
     decoding functions. */
  let decodeObj: (t<'a>, string) => t<'a>

  /* [decodeArry t s f] is the decoder produced by decoding array [s] in [t].
   Elements of the array must be decoded. */
  let decodeArry: (t<'a>, string, value => 'b) => array<'b>

  /* [done t] is some effectful action upon the decoder peformed after decoding
   is finished */
  let done: (t<'a>, string => unit) => model
}

module Make = (M: Model): (S with type model = M.t) => {
  open Belt
  open Js.Json

  type model = M.t
  type value = Js.Json.t

  type t<'a> = {
    json: Js.Dict.t<value>,
    model: M.t,
    mutable errors: array<string>,
  }

  // Error constructors
  let wrongType = k => Error(`Key: ${k} is not of the expected type.`)
  let notFound = k => Error(`Key: ${k} not found.`)

  let toBool = v =>
    switch Js.Json.decodeBoolean(v) {
    | Some(b) => b
    | None => false
    }

  let toStr = v =>
    switch Js.Json.decodeString(v) {
    | Some(str) => str
    | None => ""
    }

  let parse = json => {
    let parsed = try Js.Json.parseExn(json) catch {
    | _ => failwith(`Error parsing JSON: ${json}`)
    }

    switch classify(parsed) {
    | JSONObject(obj) => {json: obj, model: M.empty, errors: []}
    | _ => failwith(`Invalid Object: ${json}`)
    }
  }

  let getKey = (json, key) =>
    switch Js.Dict.get(json, key) {
    | Some(val) => Ok(val)
    | None => notFound(key)
    }

  let decodeVal = (r, key, fn) => {
    Result.flatMap(r, x =>
      switch fn(x) {
      | Some(val) => Ok(val)
      | None => wrongType(key)
      }
    )
  }

  let pushError = (errors, e) => {
    let _ = Js.Array2.push(errors, e)
  }

  // Boolean values
  let decodeBool = (dec, key) =>
    switch getKey(dec.json, key)->decodeVal(key, decodeBoolean) {
    | Ok(b) => b
    | Error(e) => {
        pushError(dec.errors, e)
        false
      }
    }

  // Numeric values
  let decodeNum = (dec, key) =>
    switch getKey(dec.json, key)->decodeVal(key, decodeNumber) {
    | Ok(num) => num
    | Error(e) => {
        pushError(dec.errors, e)
        0.
      }
    }

  // String values
  let decodeStr = (dec, key) =>
    switch getKey(dec.json, key)->decodeVal(key, decodeString) {
    | Ok(str) => str
    | Error(e) => {
        pushError(dec.errors, e)
        ""
      }
    }

  // Close over a newly parsed JSON dictionary
  let decodeObj = (dec, key) => {
    switch getKey(dec.json, key)->decodeVal(key, decodeObject) {
    // Create a new decoder pipeline using the values contained within the object
    | Ok(dict) => {json: dict, model: dec.model, errors: []}
    | Error(e) => {
        pushError(dec.errors, e)
        dec
      }
    }
  }

  let decodeArry = (dec, key, decoderFn) => {
    switch getKey(dec.json, key)->decodeVal(key, decodeArray) {
    | Ok(arry) => Js.Array2.map(arry, decoderFn)
    | Error(e) => {
        pushError(dec.errors, e)
        []
      }
    }
  }

  let formatErrors = errs =>
    Js.Array2.reduce(
      errs,
      (acc, err) => {`[E] ${err}\n${acc}`},
      "",
    )->String.trim

  let decode = (dec, fn) => {json: dec.json, model: fn(dec), errors: dec.errors}

  let done = ({model, errors}, fn) => {
    fn(errors->formatErrors)
    model
  }
}

// Client Code
module DataModel = {
  type nested = {e: float}

  type listF = array<bool>

  type t = {
    a: float,
    b: bool,
    c: float,
    d: nested,
    f: listF,
  }

  let empty = {
    a: 0.,
    b: false,
    c: 0.,
    d: {e: 0.},
    f: [],
  }
}

module DataModelDecoder = Make(DataModel)

let () = {
  open DataModelDecoder

  let x =
    parse(`{"a": "hi",
          "b": true,
          "c": 1,
          "d": {
            "e": 12
          },
          "f": [false, true, 13],
          "q": [1, 2, 3],
          "r": ["a", "b", "c"],
          "s": [{ "t": 1 }, {"t": 2 }]
           }`)
    ->decode(vals => {
      a: decodeNum(vals, "a"),
      b: decodeBool(vals, "b"),
      c: decodeNum(vals, "c"),
      d: {
        e: decodeObj(vals, "d")->decodeNum("e"),
      },
      f: decodeArry(vals, "f", toBool),
    })
    ->done(errs => {
      Js.log(errs)
    })

  Js.log(x)
}
