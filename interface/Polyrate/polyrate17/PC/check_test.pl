#this per script requires the diff command in Windows
#
#http://www.gnu.org/software/diffutils/diffutils.html
#

@systems=('ch4cl','ch4o','ch4oh',
'ch5','clhbr','cmc','cwmc',
'h2ni','h3','hni','ho2',
'nh3','oh3','ohcl');

@ns=(2,1,4,41,5,3,3,1,4,1,5,2,24,4);

@testruns=(
'ch4cltr1.fu15',
'ch4cltr2.fu15',
'ch4otr1.fu15',
'ch4ohtr1.fu15',
'ch4ohtr2.fu15',
'ch4ohtr3.fu15',
'ch4ohtr4.fu15',
'ch5fu29tr1.fu15',
'ch5fu29tr2.fu15',
'ch5fu30tr1.fu15',
'ch5fu30tr2.fu15',
'ch5fu30tr3.fu15',
'ch5fu30tr4.fu15',
'ch5fu30tr5.fu15',
'ch5fu30tr6.fu15',
'ch5fu31tr1.fu15',
'ch5fu40tr1.fu15',
'ch5fu40tr2.fu15',
'ch5fu40tr3.fu15',
'ch5fu40tr4.fu15',
'ch5fu40tr5.fu15',
'ch5fu40tr6.fu15',
'ch5fu50tr1.fu15',
'ch5fu50tr2.fu15',
'ch5fu50tr3.fu15',
'ch5fu51tr1.fu15',
'ch5icfu30tr1.fu15',
'ch5icfu30tr2.fu15',
'ch5icfu31tr1.fu15',
'ch5icfu40tr1.fu15',
'ch5icfu40tr2.fu15',
'ch5j1tr1.fu15',
'ch5j1tr2.fu15',
'ch5j1tr3.fu15',
'ch5j1tr4.fu15',
'ch5j1tr5.fu15',
'ch5j1tr6.fu15',
'ch5j1tr7.fu15',
'ch5j1tr8.fu15',
'ch5j1tr9.fu15',
'ch5j1tr10.fu15',
'ch5j1tr11.fu15',
'ch5j1tr12.fu15',
'ch5j2itr1.fu15',
'ch5j2itr2.fu15',
'ch5j2tr1.fu15',
'ch5j2tr2.fu15',
'ch5j2tr3.fu15',
'clhbrtr1.fu15',
'clhbrtr2.fu15',
'clhbrtr3.fu15',
'clhbrtr4.fu15',
'clhbrtr5.fu15',
'cmctr1.fu15',
'cmctr2.fu15',
'cmctr3.fu15',
'cwmctr1.fu15',
'cwmctr2.fu15',
'cwmctr3.fu15',
'h2nitr1.fu15',
'h3tr1.fu15',
'h3tr2.fu15',
'h3tr3.fu15',
'h3tr4.fu15',
'hnitr1.fu15',
'ho2tr1.fu15',
'ho2tr2.fu15',
'ho2tr3.fu15',
'ho2tr4.fu15',
'ho2tr5.fu15',
'nh3tr1.fu6',
'nh3tr2.fu15',
'oh3fu30tr1.fu15',
'oh3fu30tr2.fu15',
'oh3fu30tr3.fu15',
'oh3fu30tr4.fu6',
'oh3fu30tr5.fu6',
'oh3fu30tr6.fu15',
'oh3fu31tr1.fu15',
'oh3fu31tr2.fu15',
'oh3fu40tr1.fu15',
'oh3fu40tr2.fu15',
'oh3fu40tr3.fu15',
'oh3fu40tr4.fu6',
'oh3fu40tr5.fu6',
'oh3fu40tr6.fu15',
'oh3tr1.fu15',
'oh3tr2.fu15',
'oh3tr3.fu15',
'oh3tr4.fu15',
'oh3tr5.fu15',
'oh3tr6.fu15',
'oh3tr7.fu15',
'oh3tr8.fu15',
'oh3tr9.fu15',
'oh3tr10.fu15',
'ohcltr1.fu15',
'ohcltr2.fu15',
'ohcltr3.fu15',
'ohcltr4.fu15',
'ohcltr5.fu15');

for $i (0 .. @ns-1){
 for $j (1 .. $ns[$i]){
  	push(@system_list,$systems[$i]);
 }
}

$i=0;
foreach $system (@system_list){
	$string='diff poly_'."$system_list[$i]"
	        ."\\$testruns[$i]"
	        .' ..\\testo\\'
	        ."$testruns[$i]"
	        .' >> disc.txt';
  system("$string");
  #print "$string\n";
  $i++;  
}

