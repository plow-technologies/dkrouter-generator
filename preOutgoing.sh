cp ./dist/build/dkGenFile/dkGenFile ./
tar -czvf dkGenFile.tar.gz dkGenFile
rsync -avzhe ssh --progress dkGenFile.tar.gz node@108.168.240.123:/home/node/incoming/
rm ./dkGenFile
rm ./dkGenFile.tar.gz
