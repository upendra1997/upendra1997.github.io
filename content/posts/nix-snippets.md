---
title: "Nix Snippets"
date: 2024-03-04T18:01:01+05:30
# weight: 1
# aliases: ["/first"]
tags: ["nix"]
categories: ["nix"]
author: "Upendra Upadhyay"
# author: ["Me", "You"] # multiple authors
showToc: true
TocOpen: true
draft: true
hidemeta: false
comments: true
description: "Some nix snippets that I use"
canonicalURL: "https://canonical.url/to/page"
disableHLJS: true # to disable highlightjs
disableShare: false
disableHLJS: false
hideSummary: false
searchHidden: false
ShowReadingTime: true
ShowBreadCrumbs: true
ShowPostNavLinks: true
cover:
    image: "<image path/url>" # image path/url
    alt: "<alt text>" # alt text
    caption: "<text>" # display caption under cover
    relative: false # when using page bundles set this to true
    hidden: true # only hide on current single page
editPost:
    URL: "https://github.com/upendra1997/<path_to_repo>/blob/main/content"
    Text: "Suggest Changes" # edit text
    appendFilePath: true # to append file path to Edit link
---
I use [`direnv`](https://direnv.net/) to automatically get into flake shell as soon as I entre the directory which contain the `flake.nix`, the content of `.envrc` file is:

```
use flake
```

Most of the time I just need to use the package available in my shell for my dev work and the snipet that I use is:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
          buildInputs = with pkgs;
          [
          # package1
          # package2
          ];
        in
        with pkgs;
        {
          devShells.default = mkShell {
            inherit buildInputs;
          };
        }
      );
}
```
Sometime I write custom scrips that I need access to, I can create a nixified version using the `writeShellApplication` which is one of the [trivial builders](https://ryantm.github.io/nixpkgs/builders/trivial-builders/). Bottom is just a simple shell script to get the authentication code simlar to `google-authenticator`:

```nix
{ config, pkgs, ... }:
with pkgs;
writeShellApplication {
  name = "totp";
  runtimeInputs = with pkgs; [ cloak ];
  text = ''
    cloak view some_app | tr -d '\n' | pbcopy
  '';
}
```

and for new projects I use nix flake templates from `nix flake show templates` using `nix flake init -t 'templates#go-hello'`

---- 

I will keep adding to this post if I find someting that I use often.

