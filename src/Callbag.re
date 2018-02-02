type callbagType('callbag, 'data) =
  | Start('callbag)
  | Data('data)
  | End;

let observe = (operation, source) => {
  source(
    Start(
      t =>
        switch t {
        | Data(d) => operation(d)
        | _ => ()
        }
    )
  );
  ();
};

let interval = (period, _type) => {
  let timeoutId = ref(None);
  switch _type {
  | Start(sink) =>
    let rec startInterval = (num, callbag) => {
      timeoutId :=
        Some(
          Js.Global.setTimeout(
            () => {
              callbag(Data(num + 1));
              startInterval(num + 1, callbag);
            },
            period
          )
        );
      ();
    };
    startInterval(0, sink);
    sink(
      Start(
        _type =>
          switch _type {
          | End =>
            switch timeoutId^ {
            | Some(id) => Js.Global.clearTimeout(id)
            | None => ()
            }
          | _ => ()
          }
      )
    );
    ();
  | _ => ()
  };
};

interval(1000) |> observe(d => Js.log(d));
