module Opi = Opium.Std

type person = {
  name: string;
  age: int;
}

let upload_file headers body =
  let open Lwt.Infix in
  match Cohttp.Header.get headers "Content-Length" with
  | None -> Cohttp_lwt_unix.Server.respond_error ~status:`Length_required ~body:"Missing Content-Length\n" ()
  | Some _length ->
    let data_ = Cohttp_lwt_body.to_stream body in
    data_ |> Lwt_stream.iter_s (fun data -> Lwt.return (print_endline ("Data: " ^ data))) >>= fun _ ->
    Lwt.return ((Cohttp.Response.make ~status:`Not_found ()), (Cohttp_lwt_body.of_string "OK"))


let print_param = Opi.get "/hello/:name" begin fun req ->
  `String ("Hello " ^ Opi.param req "name") |> Opi.respond'
end

let home = Opi.get "/" begin fun _ ->
  `String ("Welcome!") |> Opi.respond'
end

let handle_upload = Opi.post "/upload" begin fun req ->
   ignore(upload_file (Opi.Request.headers req) (Opi.Request.body req));
  `String ("Welcome!") |> Opi.respond'
end

let run_server () =
  Opi.App.empty
  |> print_param
  |> home
  |> handle_upload
  |> Opi.App.port 4000
  |> Opi.App.run_command

let main () =
  print_endline "Starting server...";
  Lwt.return (run_server ())

let _ =
  Lwt_main.run (main ())
