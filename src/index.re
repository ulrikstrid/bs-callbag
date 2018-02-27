Js_Callbag.interval(100)
|> Callbag.skip(2)
|> Callbag.filter(x => x != 3)
|> Callbag.take(3)
|> Callbag.map(x => x * 15)
|> Callbag.map(Js.Int.toString)
|> Callbag.map(Js.String.charAt(0))
|> Callbag.forEach(d => Js.log(d));

Callbag.fromArray([|1, 2, 3, 4|]) |> Callbag.observe(d => Js.log(d));
