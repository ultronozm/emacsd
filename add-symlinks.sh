for i in *.el
do
  ln -s "$(pwd)"/"$i" ../"$i"
done
