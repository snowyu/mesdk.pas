= MeRemote Layer: =

== MeTransport ==
  MeStreamFormat -- Streamable the types.
MeTransportClasses -- manage the registered transport classes

== MeRemoteInvocation ==
MeRemoteInvocation
  MeRemoteObject


= MeService Layer =
MeService -- interfaced object for others
 collects the public functions provides to others.
Properties:
  //Service Name
  Class Property Name: string    
  //Service Author
  Class Property Author: string  
  Class Property Enabled: Boolean;
  Class Property MinCount: Integer;
  Class Property MaxCount: Integer;
  Class Property MaxIdleTime: Integer;
  Class Property Flags: TMeServiceFlags; //set of (msfListed, msfRunRequireAuth, msfListRequireAuth, msfStateful, msfPersistent)
  Class Property Instances: TThreadList;

  //the service instance startup time.
  Property StartupTime: TDateTime;
  Property Host: PMeServiceHost;
Methods:
  Class Method GetFunctionList(const aList: PFunctionList);

IMeServiceInfo -- the service type info.
  Property Name: string    
  Property Author: string  
  Property Enabled: Boolean;
  Property MinCount: Integer;
  Property MaxCount: Integer;
  Property MaxIdleTime: Integer;
  Property Instances: IMeServiceList;
  Method CreateService: IMeService;
IMeService -- interface only.
  //the service instance startup time.
  Property StartupTime: TDateTime;
  Property Host: IMeServiceHost;
  Property Info: IMeServiceInfo;

MeServiceClasses -- manage the registered service classes

  MeServiceHost -- host the services here.
  //the host is about shutting down 
  {
   Should be respected by the service to stop long running operations. 
   if not respected the service will eventually be killed by the host after a certain graceperiod.
  }
  Property IsShuttingDown: Boolean; 

MeServices  -- manage the service instances.
  MeServicePool

MeRemoteService

