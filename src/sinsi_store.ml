module Store =
  Irmin_mem.Make (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

module View =
  Irmin.View (Store)

let config =
  Irmin_mem.config ()

let empty =
  Store.Repo.create
