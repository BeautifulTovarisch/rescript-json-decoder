/*
  parse(`{"x": "hi", "y": { "z": 1, "t": 2 } } }`)
      ->decodeStr("x")
        ->decodeObj("y", obj =>
          decodeNum(obj, "z")
              ->decodeNum("t")
         )
        ->done(val =>
          let obj = {
              a: get(val, 'x'),
              b: {
                  c: get(val, 'y.z'),
                  d: get(val, 'y.t')
              }
            ()
        })
*/

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

  /* [decodeNum t s f] is the decoder produced by decoding number [s] in [t] */
  let decodeNum: (t<'a>, string, (model, float) => model) => t<'a>

  /* [decodeBool t s f] is the decoder produced by decoding boolean [s] in [t] */
  let decodeBool: (t<'a>, string, (model, bool) => model) => t<'a>

  /* [decodeStr t s f] is the decoder produced by decoding string [s] in [t] */
  let decodeStr: (t<'a>, string, (model, string) => model) => t<'a>

  /* [done t] is some effectful action upon the decoder peformed after decoding is finished */
  let done: (t<'a>, (model, string) => model) => model
}

module MakeJSONDecoder = (M: Model): (S with type model = M.t) => {
  open Belt
  open Js.Json

  type model = M.t

  type t<'a> = (Js.Dict.t<Js.Json.t>, model, list<string>)

  let wrongType = (k, t) => `${k} is not of type ${t}.`

  let notFound = k => `Key: ${k} not found.`

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
    | None => notFound(key)->Error
    }

  // Boolean values
  let classifyBool = (r, key) =>
    Result.flatMap(r, x =>
      switch classify(x) {
      | JSONTrue => Ok(true)
      | JSONFalse => Ok(false)
      | _ => wrongType(key, "boolean")->Error
      }
    )

  let decodeBool = ((json, obj, errors), key, fn) =>
    switch getKey(json, key)->classifyBool(key) {
    | Ok(b) => (json, fn(obj, b), errors)
    | Error(e) => (json, obj, list{e, ...errors})
    }

  // Numeric values
  let classifyNum = (r, key) =>
    Result.flatMap(r, x =>
      switch classify(x) {
      | JSONNumber(num) => Ok(num)
      | _ => wrongType(key, "number")->Error
      }
    )

  let decodeNum = ((json, obj, errors), key, fn) =>
    switch getKey(json, key)->classifyNum(key) {
    | Ok(num) => (json, fn(obj, num), errors)
    | Error(e) => (json, obj, list{e, ...errors})
    }

  let classifyString = (r, key) =>
      Result.flatMap(r, str =>
        switch classify(str) {
          | JSONString(str) => Ok(str)
          | _ => wrongType(key, "string")->Error
        }
       )

  // String values
  let decodeStr = ((json, obj, errors), key, fn) =>
      switch getKey(json, key)->classifyString(key) {
          | Ok(str) => (json, fn(obj, str), errors)
          | Error(e) => (json, obj, list{e, ...errors})
      }

  let formatErrors = errs =>
    List.reduce(errs, "", (acc, err) => {`[E] ${err}\n${acc}`})->String.trim

  let done = ((_, obj, errors), fn) => fn(obj, errors->formatErrors)
}

// Client Code
module DataModel = {
  type t = {a: float, b: bool, c: float}
  let empty = {a: 0., b: false, c: 0.}
}

module DataModelDecoder = MakeJSONDecoder(DataModel)

let () = {
  open DataModelDecoder

  let _ =
    parse(`{"a": "hi", "b": false, "c": 1}`)
    ->decodeNum("a", (obj, val) => {...obj, a: val })
    ->decodeBool("b", (obj, val) => {...obj, b: val })
    ->decodeNum("c", (obj, val) => {...obj, c: val})
    ->done((obj, errors) => {
      Js.log(errors)

      Js.log(obj)

      obj
    })
}
