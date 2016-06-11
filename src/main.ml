module Opi = Opium.Std

(* module Rss = Syndic.Rss2 *)

let feeds =
  List.map Uri.of_string ["http://gun-moll.herokuapp.com/threads?id=dang"]

let print_param = Opi.get "/hello/:name" begin fun req ->
  `String ("Hello " ^ Opi.param req "name") |> Opi.respond'
end

let home = Opi.get "/" begin fun _ ->
  `String ("Super Live(ish) reload!") |> Opi.respond'
end

let run_server () =
  Opi.App.empty
  |> print_param
  |> home
  |> Opi.App.port 4000
  |> Opi.App.run_command

let fetch_feed uri =
  let open Lwt.Infix in
  let module Client = Cohttp_lwt_unix.Client in
  Client.get uri >>= fun (_resp, body) ->
  Cohttp_lwt_body.to_string body >|= fun body ->
  Printf.printf "Uri %s has body of length: %d\n" (Uri.to_string uri) (String.length body);
  body

let fetch_and_print_feeds () =
  let open Lwt.Infix in
  print_endline "Getting feed bodies";
  let feed_bodies = List.map fetch_feed feeds in
  print_endline "Iterating over feed bodies";
  Lwt_list.iter_s (fun _body ->
      print_endline "Feed body loop";
      _body >>= fun body ->
      (* Never prints *)
      Printf.printf "\tBody: %s" body;
    Lwt.return_unit) feed_bodies

let main () =
  print_endline "Fetching feed...";
  (* Lwt.return (run_server ()) *)
  fetch_and_print_feeds ()

let _ =
  Lwt_main.run (main ())
