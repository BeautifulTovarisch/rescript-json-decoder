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
}

module type S = {
  /* [values] is the type representing the decoded values of the inputs */
  type values

  /* [model] is the data model produced after decoding is complete */
  type model

  /* [type t<'a>] is the type of decoder [t] represented by type ['a] */
  type t<'a>

  /* [parse s] is the decoder produced by parsing [s].
   Raises: (failure) if [s] is unable to be parsed */
  let parse: string => t<'a>

  /* [decodeNum t s] is the decoder produced by decoding key [s] in [t] */
  let decodeNum: (t<'a>, string) => t<'a>

  /* [getValue vs k d] is Some(vs[k]) if [k] is in [vs] or None otherwise */
  let getNum: (values, string) => float

  /* [done t] is some effectful action upon the decoder peformed after decoding is finished */
  let done: (t<'a>, (values, string) => model) => model
}

module MakeJSONDecoder = (M: Model): (S with type model = M.t) => {
  open Belt
  open Js.Json

  type model = M.t
  type values = Js.Dict.t<Js.Json.t>

  type t<'a> = (values, list<string>)

  let parse = json => {
    let parsed = try Js.Json.parseExn(json) catch {
    | _ => failwith(`Error parsing JSON: ${json}`)
    }

    switch classify(parsed) {
    | JSONObject(obj) => (obj, list{})
    | _ => failwith(`Invalid Object: ${json}`)
    }
  }

  let classifyNum = (r, key) =>
    Result.flatMap(r, x =>
      switch classify(x) {
      | JSONNumber(num) => Ok(num)
      | _ => Error(`'${key}' is not a number`)
      }
    )

  let getKey = (json, key) =>
    switch Js.Dict.get(json, key) {
    | Some(val) => Ok(val)
    | None => Error(`Key: ${key} not found.`)
    }

  let decodeNum = ((json, errors), key) =>
    switch getKey(json, key)->classifyNum(key) {
    | Ok(_) => (json, errors)
    | Error(e) => (json, list{e, ...errors})
    }

  let getNum = (json, key) =>
    switch getKey(json, key)->classifyNum(key) {
    | Ok(num) => num
    | Error(_) => 0.
    }

  let formatErrors = errs =>
    List.reduce(errs, "", (acc, err) => {`[E] ${err}\n${acc}`})->String.trim

  let done = ((vals, errors), fn) => fn(vals, errors->formatErrors)
}

// Client Code
module DataModel = {
  type t = {a: float}
}

module DataModelDecoder = MakeJSONDecoder(DataModel)

let () = {
  open DataModelDecoder
  let _ =
    parse(`{"a": 1}`)
    ->decodeNum("a")
    ->decodeNum("b")
    ->decodeNum("c")
    ->done((vals, errors) => {
      Js.log(errors)

      {a: getNum(vals, "b")}
    })
}
