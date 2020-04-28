#!/bin/bash
git add .
git commit -m "bk"
git push
if [[ "$?" != *"Done"* ]]
then
  exit 1
fi
