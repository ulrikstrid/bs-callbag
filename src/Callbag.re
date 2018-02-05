type callbagType('data) =
  | Start(callbagType('data) => unit)
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
  let num = ref(0);
  switch _type {
  | Start(sink) =>
    let intervalId =
      Js.Global.setInterval(
        () => {
          sink(Data(num^));
          num := num^ + 1;
        },
        period
      );
    sink(
      Start(
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

let take = (max, source, start) =>
  switch start {
  | Start(sink) =>
    let taken = ref(0);
    let sourceTalkback = ref(None);
    let talkback = _type =>
      switch sourceTalkback^ {
      | Some(tb) =>
        if (taken^ < max) {
          tb(_type);
        }
      | None => ()
      };
    source(
      Start(
        _type =>
          switch _type {
          | Start(callbag) =>
            sourceTalkback := Some(callbag);
            sink(Start(talkback));
          | Data(data) =>
            taken := taken^ + 1;
            sink(Data(data));
            if (taken^ == max) {
              sink(End);
              switch sourceTalkback^ {
              | Some(tb) => tb(End)
              | _ => ()
              };
            };
          | _ => sink(_type)
          }
      )
    );
  | _ => ()
  };

let filter = (pred, source, start) =>
  switch start {
  | Start(sink) =>
    source(
      Start(
        _type =>
          switch _type {
          | Start(_) => sink(_type)
          | Data(d) =>
            if (pred(d)) {
              sink(_type);
            };
            ();
          | _ => ()
          }
      )
    )
  | _ => ()
  };

let skip = (max, source, start) =>
  switch start {
  | Start(sink) =>
    let skiped = ref(0);
    source(
      Start(
        _type =>
          switch _type {
          | Start(_) => sink(_type)
          | Data(data) =>
            skiped := skiped^ + 1;
            if (max < skiped^) {
              sink(Data(data));
            };
          | _ => sink(_type)
          }
      )
    );
  | _ => ()
  };

let map = (f, source, start) =>
  switch start {
  | Start(sink) =>
    source(
      Start(
        t =>
          switch t {
          | Data(d) => sink(Data(f(d)))
          | _ => ()
          }
      )
    )
  | _ => ()
  };