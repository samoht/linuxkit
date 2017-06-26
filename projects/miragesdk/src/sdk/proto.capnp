@0x9e83562906de8259;

struct Response {
  union {
    ok       @0 :Data;
    notFound @1 :Void;
  }
}

interface Callback {
  f @0 (change :Data) -> ();
}

interface Conf {
  write  @0 (path :List(Text), data: Data) -> ();
  read   @1 (path :List(Text)) -> Response;
  delete @2 (path :List(Text)) -> ();
  watch  @3 (path :List(Text), callback :Callback) -> ();
}
