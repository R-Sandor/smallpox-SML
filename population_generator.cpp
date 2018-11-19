#include <string>
#include <iostream>
#include <list>
#include <random>
#include <ctime>
#include <fstream>
using namespace std;
#include "person.h"

int main()
{   list<int>::iterator iitr;
    list<int>::iterator ibegin;
    list<int>::iterator iend;
    list<person>pop;
    list<person>::iterator pitr;
    person * pptr;
    int age=0;
    // family id's start at 100
    int fid = 100;
    // family size
    int fsize;
    int pcount;
    // Number of people
    cout << "How many people do you wish to create? ";
    
    int Num = 1003;
    cin >> Num;  
    int Old;
    int kount=0;
    int numg;
    int x;
    // This is the range of mixing groups that a person can be in.
    int mixingGroupID= Num*3/50;


    default_random_engine Gen(time(NULL));
    ///Sdist the size of the family group
    uniform_int_distribution<int> Sdist(1,7);
    ///Gdist is the number of additional groups each belongs
    uniform_int_distribution<int> Gdist(2,4);
    ///generic mixing groups non-work/school/family/hosp
    uniform_int_distribution<int> MGdist(6000,6000+mixingGroupID);
    ///Edist returns a value greater than 0 but
    ///with 3.5 terminating at 1.0-ish
    exponential_distribution<double>Edist(3.0);


    uniform_int_distribution<int> a0dist(1,145);
    uniform_int_distribution<int> udist(1,59);
    exponential_distribution<double>  edist(.395);
    exponential_distribution<double> edist2(.25);


    for(int i=10000; i<Num+10000; i++)
    {
       pptr=new person(i);
       pop.push_back(*pptr);
    }
    pitr=pop.begin();

    while(kount<Num)
    {
    	int portion = a0dist(Gen);
        fsize=Sdist(Gen);
        for(int j=0; j<fsize; j++)
        {
        	if(portion>120){
                	if(portion >138)
     			{
                   		if(portion >143)
                    		{
                        		age = 90 + edist(Gen);
                    		}
                	    	else{

                    			age = 80 + edist(Gen);
                    		}
            		}
            		else
            		{
                   		age = 60 + edist2(Gen);
            		}

                }
    		else {age = udist(Gen);}
        	while (j == 0 && age < 18)
        	{
        		if(portion>120){
                		if(portion >138)
     				{
                    			if(portion >143)
                    			{
                        			age = 90 + edist(Gen);
                    			}
                    			else{

                    			age = 80 + edist(Gen);
                    			}
            			}
            			else
            			{
                   		age = 60 + edist2(Gen);
            			}

                		}
    				else {age = udist(Gen);}
    		}

        pitr->setFid(fid);
        pitr->setAge(age,&Gen);


        numg=Gdist(Gen);
        for(int k=0; k<numg; k++)
        {
           x = MGdist(Gen);
           pitr->add2mxg(x);
        }
        pitr->mxguniq();
        pitr->display();
        kount++;
        pitr++;
        if(pitr==pop.end()){j=fsize; kount=Num;}
        }
      fid++;
    }

    fstream fout;
    fout.open("pop_data.txt",ios::out);

    pitr=pop.begin();
    while(pitr!=pop.end())
    {
         fout<<"val population = ("<<pitr->getPid()<<", "<<pitr->getAge()<<", "<<pitr->getFid()
            <<", "<<pitr->getsymptom()<<", "<<pitr->getexposedDays()<<", "
            <<pitr->getincubateDays()<<", "<<pitr->getisDiagnosed()<<", "
            <<pitr->getdayDiesAfter()<<", " << "\"NotInfectious\" , " <<" [";

            ibegin = pitr->get_mixgrps_begin();
            iend=pitr->get_mixgrps_end();
            iitr=ibegin;
            while(iitr!=iend){
                    if(iitr!=ibegin){fout<<", "<<*iitr;}
                    else{fout<<*iitr;}
                    iitr++;}
                fout<<"])::population"<<endl;

        pitr++;
    }

    fout << "val population_size = " << Num << endl;
    return 0;


}
