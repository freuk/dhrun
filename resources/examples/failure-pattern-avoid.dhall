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
          [ "toavoid" ] : List Text
      , vars =
          [] : List { varname : Text, value : Text }
      , passvars =
          [ "PATH" ] : List Text
      , out =
          { filename =
              "out.out"
          , filecheck =
              { avoids =
                  [] : List Text
              , wants =
                  [ "something that isn't there" ] : List Text
              }
          }
      , err =
          { filename =
              "err.err"
          , filecheck =
              { avoids = [] : List Text, wants = [] : List Text }
          }
      , postchecks =
          [] : List
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
            { filename :
                Text
            , filecheck :
                { avoids : List Text, wants : List Text }
            }
        , err :
            { filename :
                Text
            , filecheck :
                { avoids : List Text, wants : List Text }
            }
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
