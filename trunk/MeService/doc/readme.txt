The MeService Library is a mini general SOA(service-oriented architecture) system framework

Purpose:
  1. Fast and Small 
  2. Mini Remote Service Library
  3. Mini Local Plugin(DLL) Library

Vocatons(词汇表):
Host:
Service:
Plugin: the plugin is a special local service for DLL.
System Plugin: 

the unit Levels:

uMeServiceTypes
  uMeService
    uMeServiceMgr
      uMePluginMgr
    uMePlugin : the plugin features implement in it(for Delphi object).
      DLL plugin MUST export 3 funcions: ServiceInfo, InitializeService,  TerminateService
      One DLL means one plugin ONLY.

plugin flowchart(流程):
  Host Load System Plugin via LoadFromDLL check three functions whether exists.
    call ServiceInfo function to retreive the plugin info.
    Initialize system plugin:
      then Fill the ServiceInitInfo
      Notity the System.OnLoaded(aPlugin) Event
      Call the System.GetPluginFolder function to get the plugin folder
      Load the plugins from the PluginFolder
      Initialize plugins
      Notity the System.OnAllLoaded() Event

  首先加载系统插件，为了验证系统插件，必须通过指定交换密钥。
    系统插件必须还有export 一个函数： exchageKey(const aKey: PAnsiChar): Integer;
    或者不导出，但是必须有 system.Version(const aKey: PAnsiChar; const aSize: Cardinal): Integer;
      所有的文字都将按指定编码被替换。返回 system 版本号.如果是空就只返回版本号。
    由系统插件决定加载哪些插件，以及最终调用各个加载插件的初始化过程。

  需要考虑的别人编写的Service api不一定是线程安全的，在多线程下如何处理？
  有一个方案是检查多线程，如果发现，就使用windows的 APC队列(QueueUserAPC)，但这要求你在空闲必须调用
  Handle := 0;
  MsgWaitForMultipleObjectsEx(0, Handle, INFINITE, QS_ALLINPUT, MWMO_ALERTABLE); 
  或者用SleepEx(1, true)设置 alertable 为真，才调用线程异步队列的函数！
  而且这样加深了平台依赖！


Service URN declaration samples:
  Function: System/Connection/UIService/Member.Enum
  Event: System/Connection.OnConnect

System Service:
  Functions:
    System.RegisterFunction
    System.RegisterEvent
    System.GetFunction(aName: TMeIdentity; const aProcParams: PTypeInfo = nil): HMeServiceFunction;
    System.GetFunctionPtr(const aFunc: TMeIdentity; const aProcInfo: Pointer= nil): Pointer;
    System.GetEvent(aName: TMeIdentity): HMeServiceEvent;
    System.CallFunction
    System.Notify(const aHandle: HMeServiceEvent; const wParam, lParam: Cardinal): Integer;
    System.NotifyByName(const aName: TMeIdentity; const wParam, lParam: Cardinal): Integer;
    System.HookEvent
    System.UnHookEvent
  Events:
    System.OnLoaded(aPlugin) : triggered when the aPlugin loaded.
    System.OnModulesLoaded() : triggered when all modules loaded.
  
Local:
  PluginMgr load plugin from DLL.
  PluginMgr ---> SerivceMgr
    manage the ServiceInfo --> CustomService 
  plugin features implementation in:  CustomPlugin --> CustomService --> AbstractService

Remote:
  1. Client: Crete RPC Plugin to do so.
       RemoteClientTransportPlugin
       RemoteClientPlugin
  2. Server: 
       RemoteServiceMgr --> PluginMgr
       RemoteServerTransportPlugin --> Plugin

过程调用支持两种模式:
  1. 内置的固定参数模式: wParam, lParam
  2. 增强模式：任意参数模式，参数传递通过 VariantArray. 暂未实现（通过编译开关打开: SUPPORTS_MESERVICE_CALLEX）

注：事件只支持固定参数模式。
