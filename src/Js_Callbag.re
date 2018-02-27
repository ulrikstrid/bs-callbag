let interval = (period, _type) => {
  let num = ref(0);
  switch _type {
  | Callbag.Start(sink) =>
    let intervalId =
      Js.Global.setInterval(
        () => {
          sink(Callbag.Data(num^));
          num := num^ + 1;
        },
        period
      );
    sink(
      Callbag.Start(
        t =>
          switch t {
          | End => Js.Global.clearInterval(intervalId)
          | _ => ()
          }
      )
    );
  | _ => ()
  };
};

let fromPromise = (source: Js.Promise.t('a), _type: Callbag.callbagType('a)) =>
  switch _type {
  | Callbag.Start(sink) =>
    Js.Promise.(
      source
      |> then_(a => {
           sink(Callbag.Data(a));
           resolve();
         })
    )
    |> ignore
  | _ => ()
  };
