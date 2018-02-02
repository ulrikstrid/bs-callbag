Callbag.interval(1000)
|> Callbag.filter(x => x != 1)
|> Callbag.take(3)
|> Callbag.observe(d => Js.log(d));
