/* Module representing a generic data model */
module type Model = {
  /* [t] is the type representing the data model */
  type t

  /* [empty] is the model of type [t] with default values */
  let empty : t
}

module type S = {
  /* [model] is the data model produced after decoding is complete */
  type model

  /* [type t<'a>] is the type of decoder [t] represented by type ['a] */
  type t<'a>

  /* [parse s] is the decoder produced by parsing [s].
   Raises: (failure) if [s] is unable to be parsed */
  let parse: string => t<'a>

  /* [decodeNum t s f] is the decoder produced by decoding number [s] in [t] and
     by application of [f] on the model and a [float]. */
  let decodeNum: (t<'a>, string, (model, float) => model) => t<'a>

  /* [decodeBool t s f] is the decoder produced by decoding boolean [s] in [t]
     and by application of [f] on the model and a [bool]. */
  let decodeBool: (t<'a>, string, (model, bool) => model) => t<'a>

  /* [decodeStr t s f] is the decoder produced by decoding string [s] in [t].
     and by application of [f] on the model and a [string]. */
  let decodeStr: (t<'a>, string, (model, string) => model) => t<'a>

  /* [decodeObj t s f] is the decoder produced by decoding object [s] in [t].
     Attributes of the object must be decoded using the other primitive
     decoding functions. */
  let decodeObj: (t<'a>, string, t<'a> => t<'a>) => t<'a>

  /* [done t] is some effectful action upon the decoder peformed after decoding
     is finished */
  let done: (t<'a>, (string) => ()) => model
}

module Make = (M: Model): (S with type model = M.t) => {
  open Belt
  open Js.Json

  type model = M.t
  type values = Js.Dict.t<Js.Json.t>

  type t<'a> = (values, model, list<string>)

  // Error constructors
  let wrongType = (k) => Error(`${k} is not of the expected type.`)
  let notFound = k => Error(`Key: ${k} not found.`)

  let parse = json => {
    let parsed = try Js.Json.parseExn(json) catch {
    | _ => failwith(`Error parsing JSON: ${json}`)
    }

    switch classify(parsed) {
    | JSONObject(obj) => (obj, M.empty, list{})
    | _ => failwith(`Invalid Object: ${json}`)
    }
  }

  let getKey = (json, key) =>
    switch Js.Dict.get(json, key) {
    | Some(val) => Ok(val)
    | None => notFound(key)
    }

  let decode = (r, key, fn) => {
    Result.flatMap(r, x =>
      switch fn(x) {
          | Some(val) => Ok(val)
          | None => wrongType(key)
      }
    )
  }

  // Boolean values
  let decodeBool = ((json, data, errors), key, fn) =>
    switch getKey(json, key)->decode(key, decodeBoolean) {
    | Ok(b) => (json, fn(data, b), errors)
    | Error(e) => (json, data, list{e, ...errors})
    }

  // Numeric values
  let decodeNum = ((json, data, errors), key, fn) =>
    switch getKey(json, key)->decode(key, decodeNumber) {
    | Ok(num) => (json, fn(data, num), errors)
    | Error(e) => (json, data, list{e, ...errors})
    }

  // String values
  let decodeStr = ((json, data, errors), key, fn) =>
      switch getKey(json, key)->decode(key, decodeString) {
          | Ok(str) => (json, fn(data, str), errors)
          | Error(e) => (json, data, list{e, ...errors})
      }

  let decodeObj = ((json, data, errors), key, fn) => {
    switch getKey(json, key)->decode(key, decodeObject) {
      | Ok(dict) => {
        // Create a new decoder pipeline using the values contained within the object
        let (_, newData, newErrors) = fn((dict, data, errors))

        (json, newData, List.concat(newErrors, errors))
      }
      | Error(e) => (json, data, list{e, ...errors})
    }
  }

  let formatErrors = errs =>
    List.reduce(errs, "", (acc, err) => {`[E] ${err}\n${acc}`})->String.trim

  let done = ((_, obj, errors), fn) => {
    fn(errors->formatErrors)
    obj
  }
}

// Client Code
module DataModel = {
  type nested = {
    e: float
  }

  type t = {
    a: float,
    b: bool,
    c: float,
    d: nested
  }

  let empty = {
    a: 0.,
    b: false,
    c: 0.,
    d: {
      e: 0.
    }
  }
}

module DataModelDecoder = Make(DataModel)

let () = {
  open DataModelDecoder

  let x =
    parse(`{"a": "hi", "b": false, "c": 1, "d": { "e": 12 } }`)
    ->decodeNum("a", (obj, val) => {...obj, a: val })
    ->decodeBool("b", (obj, val) => {...obj, b: val })
    ->decodeNum("c", (obj, val) => {...obj, c: val, d: { e: 0. }})
    ->decodeObj("d", m => {
        decodeNum(m, "e", (obj, val) => {...obj, d: { e: val } })
    })
    ->done(Js.log)

  Js.log(x)

}
