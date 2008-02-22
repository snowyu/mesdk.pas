= MeRemote Layer: =

== MeTransport ==
  MeStreamFormater -- convert the atreamable the types to any format.
MeTransportClasses -- manage the registered transport classes

TODO: transport object define.
the transport object will transfer the type data from here to there.
the transport can wrap(wrap links) the data before transfering, and unwrap it after transfered.
1.Compress;2.Encrypt;3.Data Format
消息：出版消息(publisher)，订阅消息(subscriber)
    消息 = MSG.ServiceName.Subject (非强制的，可以存在跨Service的消息)
      第一部分为消息的类型，
      出版者：某Service +  刊号： Subject,允许子刊，用"."分隔。
      订阅者列表：
      内容：
    对于远程服务而言，消息这一部分可以被放到transport中。完全和Service分离。


== MeRemoteInvoker ==
MeRemoteInvoker
  MeRemoteObject

TRemoteObject: 属性和方法都是远程
TRemoteObject  抽象的目的是为了保证远程属性的处理。需要做一些约定，比如所有以"R"打头的出版属性并且必须有设置属性的方法。
或者抽象的出版方法为远程方法，属性的写方法为抽象方法的为远程方法。如果这样就没有必要对远程对象从指定类上派生。

TRemoteInvokerFeature: 注入后的类的某些约定的抽象方法获得远程执行的能力。Singleton.
  GRemoteInvokerFeature.Transport : the default transport.
  GRemoteInvokerFeature.Add(aClass, aTransport = nil); aTransport is nil means the default transport used.

Class Specifiction:
 * the abstract published methods in the aClass are the remote invoke methods.

TRemoteObjectFeature = Class(TRemoteInvokerFeature)

Class Specifiction:
 * the remote properties
   * write to the remote [Cache mechism?]
   * read from the remote when changed.
   * confiction resolve.

属性方法注入的支持：简单属性，数组属性[]。

= MeService Layer =
MeService -- interfaced object for others
 collects the public functions and events(messages) provides to others.
Properties:
  //## ServiceInfo: 
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
  //Class Method GetFunctionList(const aList: PFunctionList); //I can use the Inventory Service to do so.

TMeServiceInfo -- the service type info: the basic service information and manage the instances of the service
  Property Name: string    
  Property Author: string  
  Property Enabled: Boolean;
  Property MinCount: Integer;
  Property MaxCount: Integer;
  Property MaxIdleTime: Integer;
  Property Instances: IMeServiceList;
  Method CreateService: IMeService;
TMeService -- //all published functions are service functions.
  //the service instance startup time.
  Property StartupTime: TDateTime;
  Property Host: IMeServiceHost;
  Property Info: IMeServiceInfo;

TMeRegisteredServices -- manage the registered service classes

  MeServiceHost -- host the services here.
  //the host is about shutting down 
  {
   Should be respected by the service to stop long running operations. 
   if not respected the service will eventually be killed by the host after a certain graceperiod.
  }
  Property IsShuttingDown: Boolean; 

MeServices  -- manage the service instances.
  MeServicePool

The MeService Library is a general service system framework.
 * TMeServiceMgr(TMeRegisteredServices): the service list, manage the service.
   the TMeServiceMgr mediates the communication between services. the services and events are registered to TMeServiceMgr.
 * TMeService: 

在本地 Service中 TMeServiceFunction 就是 Exported DLL 函数，需要定义的就是 Events。
如果是在Delphi中通过Published方法就可以不必暴露函数，那么其它语言怎么办，对于其它语言可以通过接口：只要它获得service的接口规格即可。
Event：
Keypoint：
  Spec:
    只要是出版的方法都为 ServiceFunctions?
    属性： 
      出版的事件属性为 Service 事件。注意事件属性必须以"On"打头: OnChangedEvent: TServiceEvent; [只在本地服务中实现]
        加载后由CoreService维护订阅者。
      在本地服务中，不用实现这么复杂的消息机制。就事件机制足够。
可不可以用纯 MeObject来实现MeService，然后在运行时刻动态创建和生成接口？应该是可行的。
不过如果全部用 MeObject，那么Published就用不上了。 only depend the CoreService

One Host can include mnay services.
CoreService: this service must have in the host. its functionality are:
  RegisterFunction();

CoreLocalService
  RegisterEvent()
  Subscribe/UnSubscribe Event

InventoryService: Informs clients about which services and functionality is available.
 * List all services
 * list all functions of a service 
 * list all messages of a service

MeRemoteService

CoreRemoteService
