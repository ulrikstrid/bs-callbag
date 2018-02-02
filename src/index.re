Callbag.interval(1000) |> Callbag.take(2) |> Callbag.observe(d => Js.log(d));
