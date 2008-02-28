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
  iRegisterFunction(const aProc: Pointer; const aProcType: PMeProcType); //only for delphi
  RegisterFunction(const aProc: Pointer; const aProcDesc: PChar); stdcall; //for all other language

CoreLocalService
  RegisterEvent()
  Subscribe/UnSubscribe Event

InventoryService: Informs clients about which services and functionality is available.
 * List all services
 * list all functions of a service 
 * list all messages of a service

MeRemoteService

CoreRemoteService














Interface instance variable:
[VMTAddr]  the records...  [MTAddr]
   |                          |
 TObject                   Interface

MTAddr:
  Adjust Self paramater directive
JMP XXXX
...
83442404F0 add dword ptr [esp+4], -$10 //调整Self参数指针的位置，在Delphi中为 - 实例记录长度 - 4。
JMP TInterfacedObject.QueryInterface
83442404F0 add dword ptr [esp+4], -$10
JMP TInterfacedObject._AddRef
83442404F0 add dword ptr [esp+4], -$10
JMP TInterfacedObject._Release


//Get the offset of the self object to IMT.
function GetPIMTOffset(const I: IInterface): integer;
// PIMT = Pointer to Interface Method Table
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  //adjust self pointer parameter
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte : (AdjustmentByte: shortint);
      AddLong : (AdjustmentLong: longint);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := -1;
  if Assigned(Pointer(I)) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Result := -QueryInterfaceThunk.AdjustmentByte;  //一般都是负偏移量，变成正值。
        AddLong: Result := -QueryInterfaceThunk.AdjustmentLong;
      end;
    except
      // Protect against non-Delphi or invalid interface references
    end;
end;


function GetImplementingObject(const I: IInterface): TObject;
var
  Offset: integer;
begin
  Offset := GetPIMTOffset(I);
  if Offset > 0 
  then Result := TObject(PChar(I) - Offset)
  else Result := nil;  
end;

We're still only the first step towards getting the interface GUID (or IID which is the formally correct name). Now we're able to get the PIMT offset, but the offset by itself isn't very useful. What makes it useful is that we can use the offset to compare it with the offsets stored as part of the InterfaceEntry records the compiler generates for all the interfaces a class implements. As indicated above, we can use the TObject class function called GetInterfaceTable to get a pointer to this table. With that knowledge, let's write a function that tries to find the InterfaceEntry of an interface reference.

function GetInterfaceEntry(const I: IInterface): PInterfaceEntry;
var
  Offset: integer;
  Instance: TObject;
  InterfaceTable: PInterfaceTable;
  j: integer;
  CurrentClass: TClass;
begin
  Offset := GetPIMTOffset(I);
  Instance := GetImplementingObject(I);
  if (Offset >= 0) and Assigned(Instance) then
  begin
    CurrentClass := Instance.ClassType;
    while Assigned(CurrentClass) do
    begin
      InterfaceTable := CurrentClass.GetInterfaceTable;
      if Assigned(InterfaceTable) then
        for j := 0 to InterfaceTable.EntryCount-1 do
        begin
          Result := @InterfaceTable.Entries[j];
          if Result.IOffset = Offset then
            Exit;
        end;  
      CurrentClass := CurrentClass.ClassParent
    end;    
  end;
  Result := nil;  
end;


First we use the the utility functions above to get both the object instance and the offset of the PIMT field. Then we loop across this class and all parent classes looking for an InterfaceEntry that has the same offset as our PIMT field. When we find a match, we return a pointer to the InterfaceEntry record. This record contains both the PIMT offset and the IID. Let's write a simple wrapper function to extract the IID.

function GetInterfaceIID(const I: IInterface; var IID: TGUID): boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  InterfaceEntry := GetInterfaceEntry(I);
  Result := Assigned(InterfaceEntry);
  if Result then
    IID := InterfaceEntry.IID;
end;

______________
SOA

服務導向架構(Service-Oriented Architecture，SOA) 簡介

「以客為尊」的核心概念，提供網路服務單位建構一個具彈性、可重複使用的整合性介面，加速達到網路服務提升的目標。

前言
SOA是一種架構模型，由網站服務技術等標準化元件組成，目的是為企業、學校或提供網路服務單位建構一個具彈性、可重複使用的整合性介面，促進內外部如內部應用程式、用戶、與部門(系所)等相關單位完美的溝通，盡速達到網路服務提升的目標。

何謂SOA?
我們常聴到 Information Technology (IT)產業的架構演進，由1980年代的主機(mainframe)架構，到1990年代的主從式(client server)架構，到1999年時是network centric架構，而到2004年時已複雜到所謂的 Service-Oriented Architecture架構(SOA，服務導向架構) 。此外也常聴到：如果企業不導入這個架構，企業在未來就會沒有競爭力。因此，本文將針對SOA作淺顯的簡介，也希望透過本文的介紹，對於本校網站服務技術(web services) 未來的架構有所幫助。
首先讓我們釐清一些SOA的迷思。正確來說 [1]：

   1.

      SOA不是新玩意：多年前即有資訊部門或公司成功地用SOA方式來建構、運行應用程式，且當時XML、web service都尚未提出。
   2.

      SOA不是種技術：它是種建構、組織的方法，用來建立應用程式的運行環境，以及讓學校的業務程式能以「功能化」方式發展、累積。
   3.

      就算購買最新的XML、web services產品（如開發工具、執行平台、軟體元件等），也不表示就可以建構出SOA式的應用程式。

簡單來說，SOA是一種遵循典範，是針對學校或企業內應用程式的設計、開發、佈建、管理所提出的遵循典範。從資訊技術層面而言，一個執行學校或企業業務的應用程式稱為一個獨立的「邏輯單位」，而對學校或企業營運層面而言則可稱為一項「服務」，在企業的整體運算環境中就存在著多個「獨立邏輯／業務服務」，且需要對其進行妥善設計、開發、佈建、管理等，也因此需要採行服務導向架構（SOA）。

要實現SOA，需要學校或企業的程式設計師改採「持續累積服務」的觀念與角度來開發應用程式，即便這麼做在短時間內看不到顯著好處，程式師還是必須跳脫、超越過往對應用程式的想法，改以「既有服務可否再運用？」或者是「能否沿用其他同仁開發過的服務再建構？」的觀點來面對程式開發。

SOA 主張「程式開發技術」與「程式建構方法」的交替並用，以類似傳訊溝通的作法，將數個所需的「業務服務」進行連結，以此來實現一個新的應用程式，而非「從頭開發」。透過適當的程式組構及傳訊式的程式連結，可讓學校或企業快速因應學生或用戶的需求與改變，新的應用程式只要透過「傳訊微調」即可實現，而非「重新撰寫」。

SOA 不單只是程式開發的方法論，也提供行政管理層面的依循。例如它並非是以應用程式個體為角度來進行管理，而是直接將過往程式師開發出的程式視為「服務」來管理。而對「服務」間的「互動傳訊」進行分析，SOA便可讓程式設計部門的主管瞭解何時該執行哪個業務邏輯，以及為何要執行，如此資訊管理者與分析師便可對服務程序進行最佳化調適。

SOA如何運作?
SOA服務導向架構是一種新興的系統架構模型，主要概念是針對學校或企業需求組合而成的一組軟體元件。組合的元素通常包括：軟體元件、服務及流程三個部份。當學校或企業面對外部要求時，流程負責定義外部要求的處理步驟；服務包括特定步驟的所有程式元件，而軟體元件則負責執行工作的程式。SOA 已成為現今軟體發展的重要技術，透過 SOA 讓異質系統整合變得容易，程式再使用度也提高。不必自行開發或擁有所有程式元件，發展者可以視其需要組合網路上最好的服務。不受限於特定廠商的產品功能或是平台，達到真正的開放性(Openness)。從分散式元件架構到 SOA概念上，SOA 如同物件導向、軟體元件等軟體技術一般，運用小的零組件組合成應用系統。但 SOA 強調的是如何將彼此關係鬆散的應用系統功能元件在網路上發行、組合及使用。SOA 具有下列技術特性[2]：

   1.

      分散式架構 (distributed)－SOA 的組成元件是由許多分散在網路上的系統組合而來，可能是區域網路，也可能是來自廣域網路。例如網站服務技術 (web services) 就是運作 HTTP來相互連結的 SOA。如此的作法，也使得網站服務技術很快的就成為所有支援網際網路的系統平台均能使用的技術。
   2.

      關係鬆散的界面 (loosely coupled)－傳統的系統主要是將應用系統功能需求切割成相互關聯的小零組件：模組、物件或元件，發展者要花費極大的心力了解零組件是如何設計及使用，以確保不會違反零組件連接關係限制。如此一來，若要以不同零組件替換原始設計，就成為一件困難的事。SOA 的作法是以界面標準來組合系統，只要符合界面要求，零組件可以任意替換，大幅提高系統變更的彈性度。
   3.

      依據開放的標準 (Open standard)－使用開放標準是 SOA 的核心特色，過去的軟體元件平台如 CORBA、DCOM、RMI、J2EE 採用專屬協定作為元件連結的規範，使得不同平台的元件無法相通。SOA 則著重於標準與互動性，將可避免不同平台 (.NET web services 與 Java web services) 開發程式間相互整合的困擾。
   4.

      以流程角度出發 (process centric)－在建構系統時，首先了解特定工作的流程要求，並將其切割成服務界面(包括輸入與輸出資料格式)，如此其他的發展者就可以依據服務界面開發 (或選擇) 合適的元件來完成工作。

最後舉一個學校常用的例子來說明SOA在實際應用上帶來的可能性。假設我們要建立一個線上投稿的網站，網站提供的服務包括了線上投稿作業、論文分派作業、論文審查作業、線上註冊及報名作業等。傳統的方式我們會儘快找一個類似的網站，再儘快將其他類似網站的原始碼(source code)拿來修改，但其他類似網站的原始碼所執行的平台有可能不是架站者所熟悉的作業系統。若要讓架站者作一個客製化，並符合自己投稿主題的線上投稿的網站，可能要熟悉這個平台並修改網頁及測試，再加上別人的網站可能有一些bug，如果要做到毫無問題，除錯時間可能要花上三個月的時間。但是，如果我們導入SOA的架構的話，可能將來只要花費心力將作業服務模組化、物件化或元件化，然後將它們整合到網站中即可，不需要再花費時間和資源自己去維護一個線上投稿的網站，也不需要再自行建立和資料庫連結、報名付款機制等。這個網站就像是建立在SOA上，整合了這些web services元件的一個應用程式系統。更重要的是，透過如http、XML、SOAP 等產業標準開放式協定，不必擔心這些服務使用甚麼平台、甚麼技術來建立，而將來如果有更好的服務或服務提供者時，也可以輕易的將服務更換或更新。對系統開發者來講，可以快速輕鬆的將系統建構完成，將心思專注在規劃更好、更完善的系統上；對服務提供者而言，只要能設計出一個好的服務，它的潛在使用者市場將不再受到使用者平台的限制而有無限的可能。單就這類應用所呈現的美好遠景，應該可以解釋為什麼會到處聽到有人在談論SOA了。

因此 SOA 的實作，就是將所有程式邏輯及服務內容全部包裹在服務內部，並實作一個標準的介面與外部作溝通，這種做法跟傳統的元件導向做法非常類似，唯一的差別是介面定義的方式、資料格式、與溝通管道必須是產業標準 (http、XML、SOAP 等)。 也就是說只要能實作出這樣的介面，不論介面後面是什麼，都可使成為 SOA。
　

結論
綜合以上的介紹，SOA能帶來的幫助，有以下好處：
1.增加企業盈收，或提升學校的服務品質。
2.提供可變動的網路服務型態。
3.降低學校或企業的成本。
4.降低開發服務的時間。
5.整合學校或企業的網路服務技術資源。
6.降低整體風險及意外。
　

參考文獻
[1] http://dev2dev.bea.com.tw/techdoc/07soa/07soa_040812_01.htm
[2] http://www.microsoft.com/taiwan/msdn/columns/soa/SOA_overview_2004112901.htm

