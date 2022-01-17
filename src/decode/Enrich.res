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

module type Decoder = {
  type t

  /* [parse s] is the decoder produced by parsing [s].
   Raises: (failure) if [s] is unable to be parsed */
  let parse: string => t

  /* [decodeNum t s] is the decoder produced by decoding key [s] in [t] */
  let decodeNum: (t, string) => t

  /* [done t] is some effectful action upon the decoder peformed after decoding is finished */
  let done: (t) => unit
}

module JSONDecoder: Decoder = {
  open Belt
  open Js.Json
  type t = (Js.Dict.t<Js.Json.t>, Map.String.t<Js.Json.t>, string)

  let parse = json => {
    let parsed = try Js.Json.parseExn(json) catch {
    | _ => failwith(`Error parsing JSON: ${json}`)
    }

    switch classify(parsed) {
    | JSONObject(obj) => (obj, Map.String.empty, "")
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

  let decodeNum = ((json, vals, errors), key) =>
    switch getKey(json, key)->classifyNum(key) {
    | Ok(num) => (json, Map.String.set(vals, key, number(num)), errors)
    | Error(e) => (json, vals, errors ++ e)
    }

  let done = t => {
  	let (_, vals, _) = t
	Js.log(vals)
  }
}

let () = {
  	open JSONDecoder
	parse(`{"a": 1}`)
    ->decodeNum("a")
    ->decodeNum("b")
    ->done
}
