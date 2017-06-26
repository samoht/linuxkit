module Flow: sig

  module FIFO: sig
    include Mirage_flow_lwt.S
    val connect: string -> flow Lwt.t
  end

  module Socket: sig
    include Mirage_flow_lwt.S
    val connect: string -> flow Lwt.t
  end

  module Rawlink: sig
    include Mirage_flow_lwt.S
    val connect: filter:string -> string -> flow Lwt.t
  end

end

module Net: sig

  module FIFO: sig
    include Mirage_net_lwt.S
    val connect: ?mac:Macaddr.t -> string -> t Lwt.t
  end

end

module Time: Mirage_time_lwt.S
