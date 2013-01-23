#!/bin/bash
#
#
#ghc --make -o www/hello.cgi src/hello.hs
scp -i /etc/fileformatnet/nfsnet.pem src/*.hs fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net:/home/tmp
ssh -i /etc/fileformatnet/nfsnet.pem fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net "cd /home; ghc --make -o public/index.cgi tmp/index.hs"
ssh -i /etc/fileformatnet/nfsnet.pem fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net "cd /home; ghc --make -o public/hello.cgi tmp/hello.hs"
scp -i /etc/fileformatnet/nfsnet.pem www/* fileformat_regexplanet-haskell@ssh.phx.nearlyfreespeech.net:/home/public
