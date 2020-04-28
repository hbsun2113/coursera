#!/bin/bash
git add .
git commit -m "bk"
git push
if [[ "$?" != *"Done"* ]]
then
  exit
fi
