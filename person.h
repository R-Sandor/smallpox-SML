///* persons have
///PID 1000 - 6000
///Age   avg 30, std = 15
///FID  100-999, family size = 1 to 7 (uniform)
///symtom = 'notex'
///exposedDays = 0
///incubateDays = 0
///isDiagnosed = 'false'
///dayDiesAfter = 0
///Mix group list [various]
///visited list []

class person
{
public:
    person(){}
    person(int i){PID=i; symptom="\"notex\"";
                 exposedDays=0;
                 incubateDays=0;
                 isDiagnosed="\"false\"";
                 dayDiesAfter = 0;
                 }
    void setFid(int f){Fid = f; mixgrps.push_back(Fid);}
    void mxguniq(){mixgrps.sort(); mixgrps.unique();}
    void add2mxg(int i){mixgrps.push_back(i);}
    void setAge(int a, default_random_engine * gptr)
                    {Age = a;
                    int school;
                    int hosp;
                    int work;
                    uniform_int_distribution<int> elemdist(4,11);
                    uniform_int_distribution<int> msdist(2,3);
                    uniform_int_distribution<int> dice(0,100);
                    ///currently set to 23-13 = 10 mixing work groups
                    uniform_int_distribution<int> wdist(13,23);
                    if(a<12){
                            school = elemdist(*gptr);
                            mixgrps.push_back(school);
                            }
                    else if(a<15){
                            school = msdist(*gptr);
                            mixgrps.push_back(school);
                            }
                    else if(a<19)
                            {
                            mixgrps.push_back(1);
                            }
                    else if(a<65)
                            {
                            hosp=dice(*gptr);
                            if(hosp<=4){mixgrps.push_back(12);}
                            else{
                                work = wdist(*gptr);
                                mixgrps.push_back(work);
                                }
                            }


                        }
    int getPid(){return PID;}
    void display(){
        cout<<"person("<<PID<<", "<<Age<<", "<<Fid
            <<", "<<symptom<<", "<<exposedDays<<", "
            <<incubateDays<<", "<<isDiagnosed<<", "
            <<dayDiesAfter<<", [";
            iitr=mixgrps.begin();
            while(iitr!=mixgrps.end()){
                    if(iitr!=mixgrps.begin()){cout<<", "<<*iitr;}
                    else{cout<<*iitr;}
                    iitr++;}
                cout<<"], [])"<<endl;

        }
    list<int>::iterator get_mixgrps_begin(){return mixgrps.begin();}
    list<int>::iterator get_mixgrps_end(){return mixgrps.end();}
    int getFid(){return Fid;}
    int getAge(){return Age;}
    string getsymptom(){return symptom;}
    int getexposedDays(){return exposedDays;}
    int getincubateDays(){return incubateDays;}
    string getisDiagnosed(){return isDiagnosed;}
    int getdayDiesAfter(){return dayDiesAfter;}
private:
    int PID;
    int Fid;
    int Age;
    string symptom;
    int exposedDays;
    int incubateDays;
    string isDiagnosed;
    int dayDiesAfter;
    list<int>mixgrps;
    list<int>visited;
    list<int>::iterator iitr;
};
