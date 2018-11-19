(* Author: Raphael J. Sandor *)
(* Class: CS355 - Principles of Programmming langauges *)
(* Professor: J. Morris*)
(*Last edited 10/10/2017 *)
(* Purpose: To run a simulation of what *)
(* a smallpox outbreak would do to a small population *)


(* Starts population off as an empty list*)
val population = [];
val deadPeople = [];
val immuneList = [];
val infectedPeople = [];
val infectiousInfected = [];
val infectiousPeople=[];
val days = 0.0; 
(* Tried to set it up to use directory not sure if this works*)
val path = OS.FileSys.getDir();
val path = path^"/out.txt";

(* This the population data 
generated from the generate population.cpp*) 
use "pop_data.txt";


(* this allows us to extract the list from every person 
* without having to use # sign as this is more trouble 
* then it worth. *)
type aPerson = int * int * int * string * int * int * string * int * int list;
type aPopulation = (int * int * int * string * int * int * string * int * string *int list) list;



(*useful functions for retrieving information about a person *
 *all of which take a person (hd population) as  an arguement*)
(*************************************************************)
fun getGroup(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string,  mixingGroups:int list) =
  mixingGroups; 
fun getPID(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  pid; 
fun getAge(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int,infected:string, mixingGroups:int list) =
  age; 
fun getFID(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  FID; 
fun getExposure(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  exposure; 
fun getDaysSinceExposure(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int , infected:string,  mixingGroups:int list) =
  daySinceExposure; 
  fun getDaysOfIncubation(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  daysOfIncubation; 
  fun getDiagnosed(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int,infected:string,  mixingGroups:int list) =
  diagnosed; 
fun getDaysUntilDead(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  dayUntilDead; 
fun getInfected(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  infected; 
fun getMixGroups(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  mixingGroups; 
(*******************************************************************)
(* This function goes through all the people                                         *) 
(* and allows their groups to be added to a list of mixing groups                    *)
(* Definition: fun goThroughGroups (population: aPopulation , x:int, mixgrps:int list*)
(*************************************************************************************)

fun goThroughGroups (population, x:int, mixgrps:int list) = 
  if x <= 0 then mixgrps
  else 
let
  val person = (hd population)
  val aGroup = getGroup person
  val remaining = (tl population)
  val mixingGroups = aGroup@mixgrps
  val newX= x-1
in 
  goThroughGroups (remaining, newX, mixingGroups)
end;
  
val mixingGroups = goThroughGroups (population, population_size, []);

(* provides the remove duplicate function *)
fun isolate [] = []
  | isolate (l as x::xs) =
      let fun remove (x,[]) = []
            | remove (x,l as y::ys) = if x = y
                                      then remove(x,ys)
                                      else y::remove(x,ys)
      in
        x::isolate(remove(x,xs))
        end;


(* merge sort because lists in order are nice *)
fun merge (nil, ys) = ys
|   merge (xs, nil) = xs
|   merge (x::xs, y::ys) =
      if (x < y) then x :: merge(xs, y::ys)
      else y :: merge(x::xs, ys);



fun mergeSort nil = nil
|   mergeSort [e] = [e]
|   mergeSort theList =
      let
        (* From the given list make a pair of lists
         * (x,y), where half the elements of the
         * original are in x and half are in y. *)
        fun halve nil = (nil, nil)
        |   halve [a] = ([a], nil)
        |   halve (a::b::cs) =
              let
                val (x, y) = halve cs
              in
                (a::x, b::y)
              end;
        val (x, y) = halve theList
      in
        merge(mergeSort x, mergeSort y)
      end;

(* the isolate function removes duplicates in a list *)
val mixingGroups = isolate mixingGroups;
val mixingGroups = mergeSort mixingGroups;

(* Count the number of groups that are available *)
(* Takes a list as an argument *)
(* Definition fun listCounter (mixingGroups:int list) *) 
fun listCounter (mixingGroups) = 
  if mixingGroups = []  then 0
  else 
    1 + listCounter(tl mixingGroups); 

(* lets the users know the number of groups in the population *)
val numberOfGroups = listCounter mixingGroups; 

(* this allows for data to be written out to a file *) 
fun writeFile filename content =
    let val fd = TextIO.openAppend filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end;

(* Turns mixing groups in to a list of lists *)
val mixingGroups = mixingGroups::[]; 
(* This function allows every person to get added to a group by creating the    *)
(* lists                                                                        *) 
(* Defintion fun createGroups(i:int , listOfGroups:int list ( hd mixingGroups)  *)
fun createGroups (i:int , listOfGroups )= 
if  i = 0   then listOfGroups  
else
        let
        val listOfGroups =listOfGroups@[[]]; 
        
        val i = i -1;    
        in  createGroups(i, listOfGroups)
end;
    
val mixingGroups = createGroups (numberOfGroups,mixingGroups );

(* finds the location of a group in a mixingGroups                                     *)
(* Take a group that is being searched for, a location, and an another list            *)
(* Returns an int which is a location of the group inside of list                      *)
(* allows the user to decide to count from one or zero as there are times where both   *)
(* maybe a approriate, e.g. moving one past a list that you searched in side of to get *)
(* the location of a list.                                                             *)
(* Definition fun lookupList (orgGrpNumber:int, counter:int, aGroup:int list)          *)
fun lookupList (orgGrpNumber:int, counter:int, mixingGroups:int
  list ) =  
  if orgGrpNumber = (hd mixingGroups)  then counter  
  else 
    let 
    val counter = counter + 1
    val current = hd mixingGroups
    in lookupList(orgGrpNumber,  counter, (tl mixingGroups)) 
    end;

(* returns the PID of a person in a population *)
(* taks a person as an arguement               *) 
fun getPID(pid:int, age:int, FID:int,  exposure:string, daySinceExposure:int,
  daysOfIncubation:int, diagnosed:string, dayUntilDead:int, infected:string, mixingGroups:int list) =
  pid; 

(* this function adds the person to each of the groups 
* that the person belongs *)
fun getListInList (position:int, aList) =
  if position = 0 then hd(aList) 
  else
    let  
     val x = position - 1;
     val remaining= tl(aList); 
     in getListInList (x, remaining)
  end;   


(* This counts the number of lists that are in a list *) 
fun listOfListCounter (input:int list list, x:int) = 
  if input =  []  then x
  else 
  	let 
    	val input = (tl (input));
    	val x =x+1

    in listOfListCounter(input, x)
    end; 

(* This adds a single person to a group *)
fun getFrontOfList (position:int , inputlist:int list list , outputList:int list list  ) =
	if position < 1 then outputList  
	else
		let  
			val addToOutPut = hd(tl(inputlist));
			val inputlist = tl(inputlist); 
			val outputList = outputList@[addToOutPut];
			val position = position -1; 
		in getFrontOfList(position, inputlist, outputList)
	end; 

fun getBackOfList (position:int , inputlist:int list list , outputList:int list list  ) =
	if position >= listOfListCounter(inputlist, 0) then outputList  
	else
		let  
			val addList = getListInList(position, inputlist);
			val outputList = outputList@[addList];
			val position = position +1; 


		in getBackOfList(position, inputlist, outputList)
	end; 

fun addSinglePersonToGroup (pid:int, mixGrp:int list, returnedList:int list
  list )=
  if mixGrp= [] then returnedList  
  else 
    let 
      val nextGroupToAdd= hd(mixGrp);
      val mixGrp = tl(mixGrp);
      val x = 1; 
      val pos = lookupList(nextGroupToAdd, x,(hd( returnedList)));
      val frontPrt = getFrontOfList (pos-1, returnedList, [getListInList(0,returnedList)]);
      val backPrt = getBackOfList (pos+1, returnedList, []); 
      val returnedList = getListInList(pos, returnedList)@[pid];
      val returnedList = frontPrt@[returnedList]@backPrt;
    in 
      addSinglePersonToGroup(pid, mixGrp, returnedList )
    end;      

(*This adds everyone in the population to a group using add single person.. *) 
fun addPeopleToGroups (population, allMixingGrps) = 
  if   population = [] then allMixingGrps
  else let 
    val personalMixGrp =  getGroup (hd population)
    val pid = getPID (hd population)
    val allMixingGrps =  addSinglePersonToGroup (pid, personalMixGrp, allMixingGrps)
    val remaining = tl(population);
    in addPeopleToGroups (remaining, allMixingGrps)
   end;  
val mixingGroups = addPeopleToGroups (population, mixingGroups);

(* This creates a random in the range of size of the group *) 
val r = Random.rand(1,1);

fun getRandom (r) =
let 
		val nextInt = Random.randRange(1,numberOfGroups)
in
		(nextInt r)
end;
val selectedId = getRandom r; 

(* selects a group that will be infected *)
(* The group will have 1% of the population in the group *) 
fun selectGroupToInfect(aRand:int) = 
if listCounter((getListInList(aRand, mixingGroups))) >= population_size div 100 then aRand
else 
	let
			val aRand = getRandom r; 
		in (aRand)
	end; 
	val selectedId= selectGroupToInfect(selectedId);
val contaminated = getListInList(selectedId, mixingGroups);
val days=0; 

(* This returns the population after being checked *)
fun foundMember (contaminatedPpl:int list, population, location:int)=
  if (getPID(hd( population))) = (hd (contaminatedPpl)) then location
  else 
    let 
      val location = location +1;
      val population = tl(population); 
    in foundMember(contaminatedPpl, population, location)
  end; 

      
      
fun foundPerson (contaminatedGroup:int list, population, location:int)=
  if (getPID(hd( population))) = (hd (contaminatedGroup)) then location
  else 
    let 
      val location = location +1;
      val population = tl(population); 
    in foundPerson(contaminatedGroup, population, location)
  end; 
fun populationListCounter(input, x:int) = 
  if input =  []  then x
  else 
  	let 
    	val input = (tl (input));
    	val x =x+1

    in populationListCounter(input, x)
    end; 
fun backOfPopulation(position:int , inputlist:(int * int * int * string * int * int * 
	string * int * string * int list) list, outputList ) = 
	if position >= populationListCounter(inputlist, 0) then outputList  
	else
		let  
			val addList = getListInList(position, inputlist);
			val outputList = outputList@[addList];
			val position = position +1; 
		in backOfPopulation(position, inputlist, outputList)
	end; 
fun getFrontOfPopulation (position:int , inputlist:(int * int * int * string * int * int * 
	string * int * string * int list) list, outputList ) =
	if position < 1 then outputList  
	else
		let  
			val addToOutPut = hd(inputlist);
			val inputlist = tl(inputlist); 
			val outputList = outputList@[addToOutPut];
			val position = position -1; 
		in getFrontOfPopulation(position, inputlist, outputList)
	end; 

fun contaminatePerson (location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
      "Exposed", 0, getDaysOfIncubation person,
      getDiagnosed person, getDaysUntilDead person, "Infectious", getGroup  person);
      val x = ~1;
    in contaminatePerson(x,[ person]) 
    end; 

fun contaminateGroup (contaminatedGroup:int list, population) = 
  if contaminatedGroup = [] then population
  else 
    let
      (* found member in contaminated (the group of people) *) 
      (* matches to the PID of a poluation list and gets a location *)
      (* so that way the list can be returned in normall order *)
      val foundLocationOfGroup = foundMember(contaminatedGroup, population,0 );
      val contaminatedPerson  =  contaminatePerson(foundLocationOfGroup, population);
      val front = getFrontOfPopulation(foundLocationOfGroup, population, []);
      val back = backOfPopulation(foundLocationOfGroup+1, population,[]); 
      val contaminatedPopulation = front@contaminatedPerson@back;
    in contaminateGroup((tl contaminatedGroup), contaminatedPopulation)
    end;   
      
      

(* adds the first contaminated individuals into population *)
val population = contaminateGroup(contaminated,population);
fun contaminatedList (status: string, personalGroups: int list,
  contaminated:int list ) = 
  if status = "Infectious" then personalGroups
  else 
    []
fun contaminatePopulation(population, localContaminatedList:int list )= 
if tl population = [] then localContaminatedList
else 
  if getExposure(hd(tl(population))) = "Exposed" then 
  let 
    val person = hd(tl(population)); 
    val status = (getExposure person);
    val personalGroups = (getMixGroups person);
    val contaminatedReturn = personalGroups@localContaminatedList;
    val remaining = tl population;    
  in contaminatePopulation(remaining, contaminatedReturn)
  end
  else 
  contaminatePopulation(tl population, localContaminatedList)
fun foundGroup(searchThroughGroup:int list , contaminatedGroup:int  , location)=
  if hd searchThroughGroup = contaminatedGroup then location
  else 
    let 
      val location = location +1;
      val searchThroughGroup = tl(searchThroughGroup); 
    in foundGroup(searchThroughGroup, contaminatedGroup, location)
  end; 

      
fun addToContaminated(contaminatedGroup:int list, population, localcontaminated) = 
  if  contaminatedGroup = [] then localcontaminated
  else 
    let 
      val locationInMixingGroup = foundGroup(hd mixingGroups,hd  contaminatedGroup ,1 );
      val localcontaminated = localcontaminated@getListInList(locationInMixingGroup,  mixingGroups);
    in addToContaminated(tl contaminatedGroup, population, localcontaminated)
    end;  
       


(* unique the people that have been contaminated *)
val contaminated = mergeSort contaminated;
val contaminated = isolate contaminated; 

(* returns a group with all the contaminated people inside *)
val contaminatedGroups = contaminatePopulation(population,[] );
val contaminatedGroups = isolate contaminatedGroups;
val contaminatedGroups = mergeSort contaminatedGroups;

(* This looks for the members of a contaminated group *)
(* After which adds those people to this of contaminated individuals*)
val contaminated = addToContaminated(contaminatedGroups, population, []);
val population = contaminateGroup(contaminated,population);


(* place contaminated people in the population *)
val population = contaminateGroup(contaminated,population);
(*use "tryToInfectPopulation.sml";*)

(* Run again to get the groups that have been added *)

(* returns a group with all the infectious people inside *)
val contaminatedGroups = contaminatePopulation(population,[] );
val contaminatedGroups = isolate contaminatedGroups;
val contaminatedGroups = mergeSort contaminatedGroups;
(* This looks for the members of a contaminated group *)
(* After which adds those people to this of contaminated individuals*)
val contaminated = addToContaminated(contaminatedGroups, population, []);
(* unique the people that have been contaminated *)
val contaminated = mergeSort contaminated;
val contaminated = isolate contaminated; 

val population = contaminateGroup(contaminated,population);


(* returns people from a group *)
(* inputGroups:int list , findFromListOfPeople:int list, returnList *)
fun peopleInGroups(groups:int list , people, returnList) =
if  groups = [] then returnList 
else 
	let  
	(* Next group to search for *)
	  val searchFor = hd groups;
	  (* location of group in the list of mixing groups *)
	  (* used to find people in a mixingGroup *)
	  (* starting at 1 at one so that the list of mixing groups is not counted 
	  that contains people inside *)
	  val location = lookupList( searchFor, 1,hd mixingGroups);
	  (* returns the list at a location //The location in the mixingroups where 
	  a group was found *)
	  val foundList = getListInList(location, mixingGroups);
	  val returnList = foundList@returnList;
	  val groups = tl groups;
	in peopleInGroups(groups, people, returnList)
end; 
(* This finds a person in the list of the population given a PID *)
fun PIDtoPerson(aPID:int , population) =
if aPID = getPID(hd population) then hd population
else 
	let val population = tl(population)
in PIDtoPerson(aPID, population) 
end; 

(* Take a list of of people and checks if the list of people match a single person:int 
from the first list provided *)
fun isInMixingGroup(aPersonsGroups:int list , contaminatedPerson:int list) = 
if  hd aPersonsGroups= hd contaminatedPerson then  [hd contaminatedPerson] 
else 
	if tl aPersonsGroups = [] then []
	else 
		let
		val remaining = tl(aPersonsGroups)
		in isInMixingGroup(remaining, contaminatedPerson)
	end;

(* Takes a list and searches if all possible people that the person comes in contact with are infectious and if 
any of the people that those people have contacted contaminated them *)
fun isInMixingGroup2(aPersonsGroups:int list , infectiousPersons:int list) = 
if aPersonsGroups = [] = false  then
	if (isSome (List.find (fn x => x = hd aPersonsGroups)infectiousPersons)) = true  
		then true
	else 
		isInMixingGroup2(tl aPersonsGroups, infectiousPersons)

else 
	false
(* returns all the people that a person shares a group with that are also contaminated
 sharedGroups(aPerson:int (pid), contaminated(list of people who are contaminated ), [[]]); *)
(* returns all the people that a person shares a group with that are also contaminated and  *)
fun sharedGroups(aPerson:int, contaminated, returnedList:int list)=
if contaminated = [] then returnedList
else 
  let
  	(* First person in the list of contamianted people *)
  	val contaminatedPerson = hd contaminated
  	(* remaininng the list group of contaminated poeple*)
  	val remaining = tl(contaminated)  
  	(* Convert the personal id passed in to the PIDtoPerson and return the whole person *)
  	val convertedPID = PIDtoPerson (aPerson, population);
  	(* Gets the groups from the person *)
  	val personalGroups = getGroup convertedPID;
  	(* Takes in the persons groups and gets a list of people *)
  	val peopleInGroups = peopleInGroups(personalGroups, contaminated, [])
  	(* sees if any of them are a contaminated person *)
    val inMixingGroups = isInMixingGroup(peopleInGroups, [contaminatedPerson]);
    (* List of people who are contaminated that share a group with the contaminated person *)
    val aReturnedList = returnedList@inMixingGroups;
  in sharedGroups(aPerson, remaining, aReturnedList)
end;

     
(*used by the the run probability function to decide if a person is in the family 
as family members increase the risk of contmaination *)
(* defination  fun isInFamily(p1, p2) (compares pids of two people)*)
(* Takes two people (p1-p2) and decides if they are in a family and returns the person if they are in a family *)
fun isInFamily(p1, p2) =
if getPID p1 = getPID p2 then true
else false 

(* runs probability on everyone in the population given there assocaition 
with people who are infected with smallpox *)
(* uses the following functions sharedGroups function, peopleInGroups, isInFamily *)
val r = Random.rand(0,100);

(* Does the same thing as contaminate group accept with the pid of a person and changes their    *
 * their status to infected and infectious                                                       *)
(* signature  fun infectIndividual (location , population) =                                     *)
fun infectIndividual (location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
      "Infected", getDaysSinceExposure person, getDaysOfIncubation person,
      getDiagnosed person, getDaysUntilDead person, "Infectious", getGroup  person);
      val x = ~1;
    in infectIndividual(x,[ person]) 
    end; 

(* runs the probability that a person may become infected given a list of infected peopl *)
fun runProbability(aPerson:int, aGroup, riskOfInfection) = 
if aGroup = [] then riskOfInfection 
else 
	let 
	val personInGroup = hd aGroup; 
	val aFullPerson = PIDtoPerson (aPerson, population);
	val personInGroup = PIDtoPerson (personInGroup, population);
	val isPersonFamilyMember = isInFamily( aFullPerson, personInGroup);
	val child = getAge personInGroup  < 18;  
	val remaining =  (tl (aGroup));
	val riskOfInfection =  (if isPersonFamilyMember = true then 
								if child = true then riskOfInfection+0.03520
								else riskOfInfection +0.01240
							else riskOfInfection+ 0.01000);
in runProbability(aPerson, remaining, riskOfInfection)
end;
fun tryToInfect (localContaminated:int list, infectedPeople:int list, days  ) = 
if localContaminated = [] then infectedPeople 
	else
		let 
			val aPerson = hd (localContaminated);
			val remaining = tl (localContaminated);  
			(* returns people who share groups with a person that is contaminated *)
			val sharedPersonalGroups = sharedGroups(aPerson, contaminated, []);
			val probablity = runProbability(aPerson, sharedPersonalGroups, 0.0);
			val nextInt = Random.randRange(0,100)
			val chance = Real.fromInt(nextInt r); 
			val infectedPeople = infectedPeople@(if (probablity*100.0) < chance+chance*days/12.0 then [aPerson] else [])
		in tryToInfect(remaining, infectedPeople, days)
	end; 
fun infectPopulation  (infectedPeople:int list, population) = 
  if infectedPeople = [] then population
  else 
    let
      (* found member in contaminated (the group of people) *) 
      (* matches to the PID of a poluation list and gets a location *)
      (* so that way the list can be returned in normall order *)
      val locationOfPerson = foundMember( [hd infectedPeople], population, 1)
      val front = getFrontOfPopulation(locationOfPerson-1, population, []);
      val back = backOfPopulation(locationOfPerson, population,[]); 
      val infectedPerson  =  infectIndividual(locationOfPerson-1, population);
      val infectedPopulation = front@infectedPerson@back;
      val remaining = tl infectedPeople
    in infectPopulation(remaining, infectedPopulation)
    end;   
      
      
(* Same functionality as getFrontOfList.sml except that it takes an int list rather than int list list *)
(* and returns an int list rather than int list list                                                   *)
(* Definition: fun getFrontOfIntList (position:int , inputlist:int list , outputList:int list   ) =    *)
fun getFrontOfIntList (position:int , inputlist:int list , outputList:int list   ) =
	if position <1 then outputList  
	else
		let  
			val addToOutPut = hd(inputlist);
			val inputlist = tl(inputlist); 
			val outputList = outputList@[addToOutPut];
			val position = position -1; 
		in getFrontOfIntList(position, inputlist, outputList)
	end; 
fun getBackOfIntList (position:int , inputlist:int list  , outputList:int list   ) =
	if position >= listCounter(inputlist) then outputList  
	else
		let  
			val addList = lookupList(hd inputlist,position, inputlist);
			val returnedPerson = List.nth(inputlist, position); 
			val outputList = outputList@[returnedPerson];
			val position = position +1; 


		in getBackOfIntList(position, inputlist, outputList)
	end; 

val infectedPeople = tryToInfect(contaminated, [], Real.fromInt(days) );
val infectedPeople = isolate infectedPeople;
val infectedPeople = mergeSort infectedPeople; 

fun removeFromList (affectedList, listToCheckAgainst, returnList ) = 
if  listToCheckAgainst = [] then returnList
else 
	if 
	(isSome (List.find (fn x => x = hd listToCheckAgainst)affectedList)) = true  
	then 
	let
	(* returns the location of the PID in the list of contaminated *)
	val location = lookupList( hd listToCheckAgainst, 0, affectedList);
	val front = getFrontOfIntList(location, affectedList, []);
	val back = getBackOfIntList(location+1, affectedList, []);
	val returnList = front@back;
	val affectedList = returnList;
	val listToCheckAgainst= tl listToCheckAgainst
	in removeFromList(affectedList  ,listToCheckAgainst, returnList)
	end 
	else 
		removeFromList(returnList, tl listToCheckAgainst, returnList)




(* places infected people in the population *)
val population =infectPopulation(infectedPeople, population); 

(* This function will remove PIDs that are in contamianted list that are also in infected list. *)
(*removeFromList(affectedList, listToCompare )*)
val contaminated = removeFromList(contaminated, infectedPeople, []); 
val infectious = addToContaminated(contaminatedGroups, population, []);
val infectious = isolate infectious;
val infectious = mergeSort infectious;
(*val population = contaminateGroup(infectious,population);
  I am not sure what this code does for me here hmm it did something that may useless now damn... *)


val infectious = mergeSort infectious;
val r = Random.rand(0,100);

fun randoms0to100 (r) =
let 
		val nextInt = Random.randRange(0, 100);
in
		(nextInt r)
end;
fun tryToKill(aPerson) = 
if getDaysOfIncubation aPerson >= 7  then  
	if 
		randoms0to100 r >90
		then
		let 
		val status = "dead";
	in status
end
	else let val status = "alive"
in 
	status
end
else let val status ="alive" 
in status
end;
fun isInPopulation(aPID:int , population) =
if population = [] then false 
else 
if aPID = getPID(hd population) then true
	else  
	let val population = tl(population)
	in isInPopulation(aPID, population) 
end; 

(* tries to kill people who are infected and are past 7 days of incubation *)
fun updateListOfDead (population, returnList)= 
if population = [] then returnList 
else 
	 if  tryToKill(hd population) = "dead" then
	let
		val dead = getPID(hd population); 
		val deadPeople = [dead]@returnList;
		in updateListOfDead(tl population ,deadPeople)
	end
	else 
		updateListOfDead(tl population, returnList)
(* removes the dead from the list *)
fun removeFromPopulation (aList:int list, population) = 
if  aList = [] then population
else 
	if 
	isInPopulation(hd aList, population) = true   
	then 
	let
	(* returns the location of the PID in the list of contaminated *)
	val location = foundMember(aList, population,0);
	val front = getFrontOfPopulation(location, population, []);
	val back = backOfPopulation(location+1, population, []);
	val population = front@back;
	in removeFromPopulation(aList, population)
	end 
	else 
		removeFromPopulation(tl aList, population)




(* given a location in a list, returns that location *)
fun getToSpot (position:int, aList) =
  if position = 0 then hd(aList) 
  else
    let  
     val x = position - 1;
     val remaining= tl(aList); 
     in getToSpot (x, remaining)
  end;   


(* Decontamiantes a person if contaminated status extends past 3 days *)
fun decontaminatePerson(location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
      "notex", 0, 0,
      getDiagnosed person, getDaysUntilDead person, "NotInfectious", getGroup  person);
      val x = ~1;
    in decontaminatePerson(x,[ person]) 
    end; 
(* changes a persons status who has been exposed for more than 3 days and not infected 
to not exposed and not infectious *)
fun changeContaminatedStatus(population, count)= 
if count >= listCounter population then population
else 
	if getExposure (getToSpot(count, population))= "Exposed" andalso getDaysSinceExposure(getToSpot(count, population))>3 
	then 
		let
		val front = getFrontOfPopulation(count, population, []);
		val back = backOfPopulation(count+1, population, []);
		val current = decontaminatePerson(count, population); 
		val population = front@current@back;
		val count =count+1
	in changeContaminatedStatus(population, count)
end
else 
	changeContaminatedStatus(population, count+1)


(* decides which people in the population are still contaminated *)
fun stillContaminated(count, population, returnList) = 
if count >= listCounter population then returnList
else 
	if getExposure (getToSpot(count, population))= "Exposed" andalso getDaysSinceExposure(getToSpot(count, population))<= 3 
	then 
		let 
		val returnList = returnList@[getPID(getToSpot(count,population))]
		in stillContaminated(count+1,population, returnList)
	end
	else 
	 stillContaminated(count+1,population, returnList)
(* puts a person's status to both infected and infectious *)
fun infectedInfectiousPerson (location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
      "Infected", getDaysSinceExposure person, getDaysOfIncubation person,
      getDiagnosed person, getDaysUntilDead person, "Infectious", getGroup  person);
      val x = ~1;
    in infectIndividual(x,[ person]) 
    end; 

(* adds one todays of incubation for infected people*)
fun addToDaysOfIncubation (location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
     getExposure person, getDaysSinceExposure person, getDaysOfIncubation person+1,
      getDiagnosed person, getDaysUntilDead person, getInfected person, getGroup  person);
      val x = ~1;
    in addToDaysOfIncubation(x,[ person]) 
    end; 

fun incrementInfectionDays(population, count)= 
if count >= listCounter population then population
else 
	if getExposure (getToSpot(count, population))= "Infected"  
	then 
		let
		val front = getFrontOfPopulation(count, population, []);
		val back = backOfPopulation(count+1, population, []);
		val current = addToDaysOfIncubation(count, population); 
		val population = front@current@back;
		val count =count+1
	in incrementInfectionDays(population, count)
end
else 
	incrementInfectionDays(population, count+1)


(* checks if a person is infected and infectious a.k.a status infected and incubation 7-14 days *)
fun infectedAndInfectious(population, count)= 
if count >= listCounter population then population
else 
	if getExposure (getToSpot(count, population))= "Infected" andalso getDaysOfIncubation(getToSpot(count, population))>=7 
	then 
		let
		val front = getFrontOfPopulation(count, population, []);
		val back = backOfPopulation(count+1, population, []);
		val current = infectedInfectiousPerson(count, population); 
		val population = front@current@back;
		val count =count+1
	in infectedAndInfectious(population, count)
end
	else 
	infectedAndInfectious(population, count+1)

(* puts a person's status to both infected and  not infectious *)
fun infectedNotInfectiousPerson (location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
      "Infected", 0, getDaysOfIncubation person,
      getDiagnosed person, getDaysUntilDead person, "NotInfectious", getGroup  person);
      val x = ~1;
    in infectedNotInfectiousPerson(x,[ person]) 
    end; 
fun infectedNotInfectious(population, count) = 
if count >= listCounter population then population
else 
	if getExposure (getToSpot(count, population))= "Infected" andalso getDaysOfIncubation(getToSpot(count, population))<7 
	then 
		let
		val front = getFrontOfPopulation(count, population, []);
		val back = backOfPopulation(count+1, population, []);
		val current = infectedNotInfectiousPerson(count, population); 
		val population = front@current@back;
		val count =count+1
	in infectedNotInfectious(population, count)
end
	else 
	infectedNotInfectious(population, count+1)

fun getInfectious(count, population, returnList) = 
if count >= listCounter population then returnList
else 
	if getInfected (getToSpot(count, population))= "Infectious" 
	then 
	let 
		val returnList = returnList@[getPID(getToSpot(count,population))]
		in getInfectious(count+1,population, returnList)
	end
	else 
		getInfectious(count+1,population,  returnList)

fun removeFromPopulation (aList:int list, population) = 
if  aList = [] then population
else 
	if 
	isInPopulation(hd aList, population) = true   
	then 
	let
	(* returns the location of the PID in the list of contaminated *)
	val location = foundMember(aList, population,0);
	val front = getFrontOfPopulation(location, population, []);
	val back = backOfPopulation(location+1, population, []);
	val population = front@back;
	in removeFromPopulation(aList, population)
	end 
	else 
		removeFromPopulation(tl aList, population)



(*sets a persons status to being immune *)
fun immune(location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
      "Immune", 0, 0,
      getDiagnosed person, getDaysUntilDead person, "NotInfectious", getGroup  person);
      val x = ~1;
    in decontaminatePerson(x,[ person]) 
    end; 
fun getImmune(count, population, returnList) = 
if count >= listCounter population then returnList
else 
	if getExposure (getToSpot(count, population))= "Immune"
	then 
		let 
		val returnList = returnList@[getPID(getToSpot(count,population))]
		in getImmune(count+1,population, returnList)
	end
	else 
	 getImmune(count+1,population, returnList)
(* if a person is past 14 days of incubation and does not die they are immune *)
fun survivedSmallpox(population, count)= 
if count >= listCounter population then population
else 
	if getExposure (getToSpot(count, population))= "Infected" andalso getDaysOfIncubation(getToSpot(count, population))>14 
	then 
		let
		val front = getFrontOfPopulation(count, population, []);
		val back = backOfPopulation(count+1, population, []);
		val current = immune(count, population); 
		val population = front@current@back;
		val count =count+1
	in survivedSmallpox (population, count)
end
else 
	survivedSmallpox(population, (count+1))

fun addToDaysOfExposure(location , population) = 
  if location< 0 then population
  else 
    let 
      val person = getListInList(location, population);
      val person = (getPID person, getAge person, getFID person,
     getExposure person, getDaysSinceExposure person+1, getDaysOfIncubation person,
      getDiagnosed person, getDaysUntilDead person, getInfected person, getGroup  person);
      val x = ~1;
    in addToDaysOfIncubation(x,[ person]) 
    end; 
fun incrementDaysOfContamination(population, count)= 
if count >= listCounter population then population
else 
	if getExposure (getToSpot(count, population))= "Exposed"   
	then 
		let
		val front = getFrontOfPopulation(count, population, []);
		val back = backOfPopulation(count+1, population, []);
		val current = addToDaysOfExposure(count, population); 
		val population = front@current@back;
		val count =count+1
	in incrementDaysOfContamination(population, count)
end
else 
	incrementDaysOfContamination(population, count+1)
fun findInfectedPeople(population, count, returnList) =
if count >= listCounter population then returnList
else 
	if getExposure (getToSpot(count, population))= "Infected"  
	then 
		let
		val current = [getPID(getToSpot(count, population))]@returnList; 
		val returnList = current;
	in findInfectedPeople(population, count+1, returnList)
end
else 
	findInfectedPeople(population, count+1, returnList)
(* use this function with infectedPeople to try to infect the population with infectious people*)
(*val infectedPeople = tryToInfect(infectiousPeople, []);*)

(* runs the simulation for 20+ after contamination *)
(* outputs a report at the bottom of the day *) 
fun infectWithInfectious(count, population, infectiousPeople) = 
if count >= (listCounter population)  then population 
else 
	if getExposure(getToSpot(count, population)) = "notex" 
	then 
	let 
		val person = getToSpot(count, population);
		val mixingGroups = getGroup person;
		val peopleInThoseGroups = sharedGroups(getPID person,infectiousPeople,[]);
		(* will return a bool *)
		val infectiousPeopleInMixingGroup = isInMixingGroup2(peopleInThoseGroups, infectiousPeople);
		val population = 
			(if infectiousPeopleInMixingGroup = true
			then 
			let 
				val front = getFrontOfPopulation(count, population, []);
				val back = backOfPopulation(count+1, population, []);
				val current = contaminatePerson(count, population); 
				val population = front@current@back;
				val count =count+1 
			in population
		end
			else
				population);
		in infectWithInfectious(count+1,population, infectiousPeople)
		end
	else infectWithInfectious(count+1, population, infectiousPeople);



fun runSimulation (days:int, population, deadPeople, infectedPeople, infectiousPeople, contaminated, immuneList) = 
if days > 30 then population
else 
	let 
	val stringDays = Int.toString(days);
	val stringContaminatedCount = Int.toString(listCounter contaminated); 
	val stringInfectedCount = Int.toString(listCounter infectedPeople); 
	val stringImmune = Int.toString(listCounter immuneList); 
	val stringDead = Int.toString(listCounter deadPeople); 
	val no= writeFile path "_______________________________________________________\n";
	val non= writeFile path "Day:"; 
	val non= writeFile path stringDays; 
	val nonReturn = writeFile path " ";
	val nonReturn = writeFile path "contaminated:";
	val nonReturn = writeFile path 	stringContaminatedCount;
	val nonReturn = writeFile path " ";
	val nonReturn = writeFile path 	"Infected:";
	val nonReturn = writeFile path 	stringInfectedCount; 
	val nonReturn = writeFile path " ";
	val nonReturn = writeFile path  "Dead:" ;
	val nonReturn = writeFile path  stringDead;
	val nonReturn = writeFile path " ";
	val nonReturn = writeFile path  "Immune:" ;
	val nonReturn = writeFile path  stringImmune;
	val nonReturn = writeFile path "\n_______________________________________________________\n";
	val days = days +1; 

	(* Do the basic adding to peoples states *)

	val population = incrementInfectionDays(population, 0 ); 
	val population = incrementDaysOfContamination(population, 0);
	val population = infectedNotInfectious(population,0); 	
	val population = survivedSmallpox(population, 0); 
	val population = changeContaminatedStatus(population, 0);
	val contaminated = stillContaminated(0, population, []);
	val immuneList = getImmune(0, population, [])@immuneList;
	val immuneList = isolate immuneList;
	val population = infectedAndInfectious(population,0); 
	val deadPeople = updateListOfDead(population, [])@deadPeople; 
	val deadPeople = isolate deadPeople; 
	val population = removeFromPopulation(deadPeople, population);
	val infectiousPeople = getInfectious(0, population, infectiousPeople)@contaminated; 


	(* moving the virus*) 
	val infectedPeople = tryToInfect(contaminated, [], Real.fromInt(days))@infectedPeople;
	val infectedPeople = isolate infectedPeople;
	val infectedPeople = removeFromList(infectedPeople,deadPeople, infectedPeople);
	val infectedPeople = removeFromList (infectedPeople, immuneList, infectedPeople);
	val deadPeople = removeFromList(deadPeople, immuneList, deadPeople);
	val population = infectWithInfectious(0,population, infectiousPeople);
	val population = infectPopulation(infectedPeople, population); 
	val contaminated = removeFromList(contaminated, infectedPeople, []); 
	
in runSimulation(days, population, deadPeople, infectedPeople, infectiousPeople, contaminated, immuneList) 
end; 
val no= writeFile path "Report of the number people in each group\n";

val aList = runSimulation(days, population, deadPeople, infectedPeople, infectiousPeople, contaminated, immuneList); 
