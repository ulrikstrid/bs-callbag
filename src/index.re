Callbag.interval(100)
|> Callbag.skip(2)
|> Callbag.filter(x => x != 3)
|> Callbag.take(3)
|> Callbag.map(x => x * 10)
|> Callbag.observe(d => Js.log(d));
