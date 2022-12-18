#!/bin/bash
## 2022-12-17
## Author: ATNewHope
## Dependicies: swaymsg, jshon
## Usage: 
## $ sway-rectfetcher <app_id>
## Output format: <x> <y> <width> <height>
##
## This bash script could get the rect (i.e. absolute coordinates and size) info
## of the window with specified app_id (the first argument) in SwayWM.
##
## If multiple window matches, the focused window will be selected.
## If no one is focused, the first one will be selected.
##
## Remove every '#' before "$pecho" in this script to get verbose output.
## LICENSE: MIT license 

pecho='echo'
appid0="\"$1\""
br=false

getwindowinfo(){
# some windows don't have the app_id at all, -Q to ignore error
#$pecho $(cat $1)
appid=$(jshon -Q -e "app_id" <$1)
focus=$(jshon -Q -e "focused" <$1)
#$pecho ......GET "app_id:" $appid "focus:" $focus
#$pecho ......id: $(jshon -Q -e "id" <$1) name: $(jshon -Q -e "name" <$1)
# When app_id is not void, and when app_id match, then
if [ ! -z $appid ]&& [ $appid == $appid0 ]; then
 # If the specific window is focused, then forcely give value to $rect
 #$pecho $appid $focus
 if [ $focus == "true" ]; then
  rect=$(jshon -e "rect" <$1); #$pecho FOCUSED TARGET FOUND.
  # else(not focused), then only give value to $rect when it has no value yet
 else
  #$pecho NOT FOCUSED TARGET FOUND
  if [ -z "$rect" ]; then rect=$(jshon -e "rect" <$1) ; #$pecho RECT EMPTY, GIVING VALUE ;
  fi
 fi
 #$pecho $rect
fi
}

# n1 is the number of nodes in depth 1 (n2, n3, ... similarly)
# i1 is the increment number in depth 1 (i1, i2, ... similarly)
# The N in "dN-nodes" means the depth.
# Following codes use a combination of "if" and "then for" (also with a "break?" at beginning), so the end of this would be "done;fi".

p0=$(mktemp); swaymsg -t get_tree > $p0; n1=$(jshon -e nodes -l <$p0);
#$pecho entering d0-node, found $n1 d1-nodes.
if [ $n1 -gt 0 ]; then for i1 in $(seq 0 $(($n1-1))); do if $br;then break;fi
  p1=$(mktemp) ;echo $(jshon -e nodes -e $i1 <$p0) > $p1 ; n2=$(jshon -e nodes -l <$p1);
  #$pecho ..entering d1-node "i1=$i1", found $n2 d2-nodes.
  if [ $n2 -gt 0 ]; then for i2 in $(seq 0 $(($n2-1))); do if $br;then break;fi
    p2=$(mktemp) ;echo $(jshon -e nodes -e $i2 <$p1) > $p2 ; n3=$(jshon -e nodes -l <$p2);
    #$pecho ..entering d2-node "i2=$i2", found $n3 d3-nodes.
    if [ $n3 -gt 0 ]; then for i3 in $(seq 0 $(($n3-1))); do if $br;then break;fi
      p3=$(mktemp) ;echo $(jshon -e nodes -e $i3 <$p2) > $p3 ; n4=$(jshon -e nodes -l <$p3);
      #$pecho ..entering d3-node "i3=$i3", found $n4 d4-nodes.
      getwindowinfo $p3
      if [ $n4 -gt 0 ]; then for i4 in $(seq 0 $(($n4-1))); do if $br;then break;fi
        p4=$(mktemp) ;echo $(jshon -e nodes -e $i4 <$p3) > $p4 ; n5=$(jshon -e nodes -l <$p4);
        #$pecho ..entering d4-node "i4=$i4", found $n5 d5-nodes.
        getwindowinfo $p4
	if [ $n5 -gt 0 ]; then for i5 in $(seq 0 $(($n5-1))); do if $br;then break;fi
	  p5=$(mktemp) ;echo $(jshon -e nodes -e $i5 <$p4) > $p5 ; n6=$(jshon -e nodes -l <$p5);
	  #$pecho ..entering d5-node "i5=$i5", found $n6 d6-nodes.
	  getwindowinfo $p5
	  if [ $n6 -gt 0 ]; then for i6 in $(seq 0 $(($n6-1))); do if $br;then break;fi
	    p6=$(mktemp) ;echo $(jshon -e nodes -e $i6 <$p5) > $p6 ; n7=$(jshon -e nodes -l <$p6);
	    #$pecho ..entering d6-node "i6=$i6", found $n7 d7-nodes.
	    getwindowinfo $p6
	  done;fi #"for i6" END
	done;fi #"for i5" END
      done;fi #"for i4" END
    done;fi #"for i3" END
  done;fi #"for i2" END
done;fi #"for i1" END
#$pecho RECT FOUND AS $rect.

if [ ! -z "$rect" ]; then 
 p=$(mktemp);echo $rect > $p
 x=$(jshon -e "x" <$p)
 y=$(jshon -e "y" <$p)
 w=$(jshon -e "width" <$p)
 h=$(jshon -e "height" <$p)
else
  notify-send "swaymsg-fetch: ERROR: No rect of $appid0 found."
# Window with app_id $appid0 may not exist, or "swaymsg -t get_tree" has updated a new structure of json, or the wanted window info in the json is too deep.
fi
#echo $x $y $w $h
echo $x $y
#notify-send "$x $y"
