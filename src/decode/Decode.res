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

  /* [done t] is some effectful action upon the decoder peformed after decoding
     is finished */
  let done: (t<'a>, (string) => ()) => model
}

module Make = (M: Model): (S with type model = M.t) => {
  open Belt
  open Js.Json

  type model = M.t

  type t<'a> = (Js.Dict.t<Js.Json.t>, model, list<string>)

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
  let decodeBool = ((json, obj, errors), key, fn) =>
    switch getKey(json, key)->decode(key, decodeBoolean) {
    | Ok(b) => (json, fn(obj, b), errors)
    | Error(e) => (json, obj, list{e, ...errors})
    }

  // Numeric values
  let decodeNum = ((json, obj, errors), key, fn) =>
    switch getKey(json, key)->decode(key, decodeNumber) {
    | Ok(num) => (json, fn(obj, num), errors)
    | Error(e) => (json, obj, list{e, ...errors})
    }

  // String values
  let decodeStr = ((json, obj, errors), key, fn) =>
      switch getKey(json, key)->decode(key, decodeString) {
          | Ok(str) => (json, fn(obj, str), errors)
          | Error(e) => (json, obj, list{e, ...errors})
      }

  // Array values
  // let classifyArry = (r, key) =>
  //     Result.flatMap(r, arr =>
  //       switch classify(arr) {
  //           | JSONArray(arr) => Ok(arr)
  //           | _ => wrongType(key, "array")
  //       }
  //     )

  // let decodeArry = ((json, obj, errors), key, fn) =>
  //     switch getKey(json, key)->classifyArry(key) {
  //         | Ok(arry) => (json, fn(obj, arry), errors)
  //         | Error(e) => (json, obj, list{e, ...errors})
  //     }

  let formatErrors = errs =>
    List.reduce(errs, "", (acc, err) => {`[E] ${err}\n${acc}`})->String.trim

  let done = ((_, obj, errors), fn) => {
    fn(errors->formatErrors)
    obj
  }
}

// Client Code
module DataModel = {
  type t = {a: float, b: bool, c: float}
  let empty = {a: 0., b: false, c: 0.}
}

module DataModelDecoder = Make(DataModel)

let () = {
  open DataModelDecoder

  let x =
    parse(`{"a": "hi", "b": false, "c": 1}`)
    ->decodeNum("a", (obj, val) => {...obj, a: val })
    ->decodeBool("b", (obj, val) => {...obj, b: val })
    ->decodeNum("c", (obj, val) => {...obj, c: val})
    ->done(Js.log)

  Js.log(x)

}
