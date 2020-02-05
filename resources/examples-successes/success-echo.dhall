-- ******************************************************************************
--  Copyright 2020 Valentin Reis.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: MIT
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ cmds =
    [ { name =
          "echo"
      , exitcode =
          Some < ExitSuccess | ExitFailure : { _1 : Integer } >.ExitSuccess
      , args =
          [ "arbitrary-character-string" ] : List Text
      , vars =
          [] : List { varname : Text, value : Text }
      , passvars =
          [ "PATH" ] : List Text
      , out =
          { filename = "out.out", filecheck = [] : List Text }
      , err =
          { filename = "err.err", filecheck = [] : List Text }
      , postchecks =
          [ { filename =
                "out.out"
            , filecheck =
                { avoids =
                    [] : List Text
                , wants =
                    [ "arbitrary-character-string" ] : List Text
                }
            }
          ] : List
              { filename :
                  Text
              , filecheck :
                  { avoids : List Text, wants : List Text }
              }
      , timeout =
          None Integer
      , otherwd =
          None Text
      }
    ] : List
        { name :
            Text
        , exitcode :
            Optional < ExitSuccess | ExitFailure : { _1 : Integer } >
        , args :
            List Text
        , vars :
            List { varname : Text, value : Text }
        , passvars :
            List Text
        , out :
            { filename : Text, filecheck : List Text }
        , err :
            { filename : Text, filecheck : List Text }
        , postchecks :
            List
            { filename :
                Text
            , filecheck :
                { avoids : List Text, wants : List Text }
            }
        , timeout :
            Optional Integer
        , otherwd :
            Optional Text
        }
, workdir =
    "./"
, cleaning =
    < Keep | Remove >.Keep
, verbosity =
    < Normal | Verbose >.Verbose
, pre =
    [] : List Text
, post =
    [] : List Text
}
