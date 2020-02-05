-- ******************************************************************************
--  Copyright 2020 Valentin Reis.
--  (c.f. AUTHORS, LICENSE)
--
--  SPDX-License-Identifier: MIT
-- ******************************************************************************
--
--     this file is generated, modifications will be erased.
--

{ cmds :
    List
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
, workdir :
    Text
, cleaning :
    < Keep | Remove >
, verbosity :
    < Normal | Verbose >
, pre :
    List Text
, post :
    List Text
}
