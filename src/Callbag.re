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
  let intervalId = ref(None);
  let num = ref(0);
  switch _type {
  | Start(sink) =>
      intervalId :=
        Some(
          Js.Global.setInterval(
            () => {
              sink(Data(num^));
              num := num^ + 1;
            },
            period
          )
        );
      sink(
        Start(
          _type =>
            switch _type {
            | End =>
              switch intervalId^ {
              | Some(id) => {
                Js.Global.clearInterval(id);
              }
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

let take = (max, source, start) =>
  switch start {
  | Start(sink) =>
    let taken = ref(0);
    let sourceTalkback = ref(None);
    let talkback = (_type) => {
      switch sourceTalkback^ {
      | Some(tb) => if (taken^ < max) tb(_type);
      | None => ();
      };
    };
    source(
      Start(
        _type =>
          switch _type {
          | Start(callbag) => {
            sourceTalkback := Some(callbag);
            sink(Start(talkback))
          }
          | Data(data) =>
            taken := taken^ + 1;
            sink(Data(data));
            if (taken^ == max) {
              sink(End);
              switch sourceTalkback^ {
              | Some(tb) => tb(End);
              | _ => ();
              };
            };
          | _ => sink(_type)
          }
      )
    );
  | _ => ()
  };

interval(1000) |> take(2) |> observe(d => Js.log(d));
