#!/bin/bash
#
# deploy to NearlyFreeSpeech
#
# copy src files
#
scp -i /etc/fileformatnet/nfsnet.pem src/*.hs fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net:/home/tmp
scp -i /etc/fileformatnet/nfsnet.pem src/*.cabal fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net:/home/tmp

#
# compile them into public
#
ssh -i /etc/fileformatnet/nfsnet.pem fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net "cd /home/tmp; cabal install --bindir=../public"

#
# copy static files
#
scp -i /etc/fileformatnet/nfsnet.pem www/* fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net:/home/public
