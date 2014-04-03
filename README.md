This is a configuration management system using Haskell and Git.

Propellor enures that the system it's run against satisfies a list of
properties, taking action as necessary when a property is not yet met.

Propellor is configured via a git repository, which typically lives
in ~/.propellor/. The git repository contains a config.hs file,
and also the entire source code to propellor.

You typically want to have the repository checked out on a laptop, in order
to make changes and push them out to hosts. Each host will also have a
clone of the repository, and in that clone "make" can be used to build and
run propellor. This can be done by a cron job (which propellor can set up),
or a remote host can be triggered to update by running propellor on your
laptop: propellor --spin $host

Properties are defined using Haskell. Edit config.hs to get started.
For API documentation, see <http://hackage.haskell.org/package/propellor/>

There is no special language as used in puppet, chef, ansible, etc.. just
the full power of Haskell. Hopefully that power can be put to good use in
making declarative properties that are powerful, nicely idempotent, and
easy to adapt to a system's special needs.

Also avoided is any form of node classification. Ie, which hosts are part
of which classes and share which configuration. It might be nice to use
reclass[1], but then again a host is configured using simply haskell code,
and so it's easy to factor out things like classes of hosts as desired.

## quick start

1. Get propellor installed
     `cabal install propellor`
          or
     `apt-get install propellor`
2. Run propellor for the first time. It will set up a `~/.propellor/` git
   repository for you.
3. In `~/.propellor/`, use git to push the repository to a central
   server (github, or your own git server). Configure that central
   server as the origin remote of the repository.
4. If you don't have a gpg private key, generate one: `gpg --gen-key`
5. Run: `propellor --add-key $KEYID`
6. Edit `~/.propellor/config.hs`, and add a host you want to manage.
   You can start by not adding any properties, or only a few.
7. Pick a host and run: `propellor --spin $HOST`
8. Now you have a simple propellor deployment, but it doesn't do
   much to the host yet, besides installing propellor.

   So, edit `~/.propellor/config.hs` to configure the host (maybe
   start with a few simple properties), and re-run step 7.
   Repeat until happy and move on to the next host. :)
9. To move beyond manually running propellor --spin against hosts
   when you change configuration, add a property to your hosts
   like: `Cron.runPropellor "30 * * * *"`
   
   Now they'll automatically update every 30 minutes, and you can
   `git commit -S` and `git push` changes that affect any number of
   hosts.
8. Write some neat new properties and send patches to <propellor@joeyh.name>!

## security

Propellor's security model is that the hosts it's used to deploy are
untrusted, and that the central git repository server is untrusted.

The only trusted machine is the laptop where you run `propellor --spin`
to connect to a remote host. And that one only because you have a ssh key
or login password to the host.

Since the hosts propellor deploys are not trusted by the central git
repository, they have to use git:// or http:// to pull from the central
git repository, rather than ssh://. 

So, to avoid a MITM attack, propellor checks that any commit it fetched
from origin is gpg signed by a trusted gpg key, and refuses to deploy it
otherwise.

That is only done when privdata/keyring.gpg exists. To set it up:

	gpg --gen-key                 # only if you don't already have a gpg key
	propellor --add-key $MYKEYID

In order to be secure from the beginning, when `propellor --spin` is used
to bootstrap propellor on a new host, it transfers the local git repositry
to the remote host over ssh. After that, the remote host knows the
gpg key, and will use it to verify git fetches.

Since the propoellor git repository is public, you can't store
in cleartext private data such as passwords, ssh private keys, etc.

Instead, `propellor --spin $host` looks for a `~/.propellor/privdata/$host.gpg` file and
if found decrypts it and sends it to the remote host using ssh. This lets
a remote host know its own private data, without seeing all the rest.

To securely store private data, use: `propellor --set $host $field`
The field name will be something like 'Password "root"'; see PrivData.hs
for available fields.

## debugging

Set `PROPELLOR_DEBUG=1` to make propellor print out all the commands it runs
and any other debug messages that Properties choose to emit.

[1] http://reclass.pantsfullofunix.net/
