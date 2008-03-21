
{ Summary: Represents the Uniform Resource Identifier object. }
{ Description
 An instance of this class represents a URI reference as defined by RFC 2396(http://www.ietf.org/rfc/rfc2396.txt): Uniform Resource Identifiers (URI): Generic Syntax, amended by RFC 2732: Format for Literal IPv6 Addresses in URLs and with the minor deviations noted below. This class provides constructors for creating URI instances from their components or by parsing their string forms, methods for accessing the various components of an instance, and methods for normalizing, resolving, and relativizing URI instances. Instances of this class are immutable.

URI syntax and components
At the highest level a URI reference (hereinafter simply "URI") in string form has the syntax

    [scheme:]scheme-specific-part[#fragment] 

where square brackets [...] delineate optional components and the characters : and # stand for themselves.

An absolute URI specifies a scheme; a URI that is not absolute is said to be relative. URIs are also classified according to whether they are opaque or hierarchical.

An opaque URI is an absolute URI whose scheme-specific part does not begin with a slash character ('/'). Opaque URIs are not subject to further parsing. Some examples of opaque URIs are:

    mailto:java-net@java.sun.com	
    news:comp.lang.java	
    urn:isbn:096139210x

A hierarchical URI is either an absolute URI whose scheme-specific part begins with a slash character, or a relative URI, that is, a URI that does not specify a scheme. Some examples of hierarchical URIs are:

    http://java.sun.com/j2se/1.3/
    docs/guide/collections/designfaq.html#28 ../../../demo/jfc/SwingSet2/src/SwingSet2.java file:///~/calendar 

A hierarchical URI is subject to further parsing according to the syntax

    [scheme:][//authority][path][?query][#fragment] 

where the characters :, /, ?, and # stand for themselves. The scheme-specific part of a hierarchical URI consists of the characters between the scheme and fragment components.

The authority component of a hierarchical URI is, if specified, either server-based or registry-based. A server-based authority parses according to the familiar syntax

    [user-info@]host[:port] 

where the characters @ and : stand for themselves. Nearly all URI schemes currently in use are server-based. An authority component that does not parse in this way is considered to be registry-based.

The path component of a hierarchical URI is itself said to be absolute if it begins with a slash character ('/'); otherwise it is relative. The path of a hierarchical URI that is either absolute or specifies an authority is always absolute.

All told, then, a URI instance has the following nine components:

    Component              Type
    scheme                 String
    scheme-specific-part   String
    authority              String
    user-info              String
    host                   String
    port                   int
    path                   String
    query                  String
    fragment               String

In a given instance any particular component is either undefined or defined with a distinct value. Undefined string components are represented by null, while undefined integer components are represented by -1. A string component may be defined to have the empty string as its value; this is not equivalent to that component being undefined.

Whether a particular component is or is not defined in an instance depends upon the type of the URI being represented. An absolute URI has a scheme component. An opaque URI has a scheme, a scheme-specific part, and possibly a fragment, but has no other components. A hierarchical URI always has a path (though it may be empty) and a scheme-specific-part (which at least contains the path), and may have any of the other components. If the authority component is present and is server-based then the host component will be defined and the user-information and port components may be defined.
Operations on URI instances
The key operations supported by this class are those of normalization, resolution, and relativization.

Normalization is the process of removing unnecessary "." and ".." segments from the path component of a hierarchical URI. Each "." segment is simply removed. A ".." segment is removed only if it is preceded by a non-".." segment. Normalization has no effect upon opaque URIs.

Resolution is the process of resolving one URI against another, base URI. The resulting URI is constructed from components of both URIs in the manner specified by RFC 2396, taking components from the base URI for those not specified in the original. For hierarchical URIs, the path of the original is resolved against the path of the base and then normalized. The result, for example, of resolving

    docs/guide/collections/designfaq.html#28          (1) 

against the base URI http://java.sun.com/j2se/1.3/ is the result URI

    http://java.sun.com/j2se/1.3/docs/guide/collections/designfaq.html#28 

Resolving the relative URI

    ../../../demo/jfc/SwingSet2/src/SwingSet2.java    (2) 

against this result yields, in turn,

    http://java.sun.com/j2se/1.3/demo/jfc/SwingSet2/src/SwingSet2.java 

Resolution of both absolute and relative URIs, and of both absolute and relative paths in the case of hierarchical URIs, is supported. Resolving the URI file:///~calendar against any other URI simply yields the original URI, since it is absolute. Resolving the relative URI (2) above against the relative base URI (1) yields the normalized, but still relative, URI

    demo/jfc/SwingSet2/src/SwingSet2.java 

Relativization, finally, is the inverse of resolution: For any two normalized URIs u and v,

    u.relativize(u.resolve(v)).equals(v)  and
    u.resolve(u.relativize(v)).equals(v)  .

This operation is often useful when constructing a document containing URIs that must be made relative to the base URI of the document wherever possible. For example, relativizing the URI

    http://java.sun.com/j2se/1.3/docs/guide/index.html 

against the base URI

    http://java.sun.com/j2se/1.3 

yields the relative URI docs/guide/index.html.
Character categories
RFC 2396 specifies precisely which characters are permitted in the various components of a URI reference. The following categories, most of which are taken from that specification, are used below to describe these constraints:

    alpha 	The US-ASCII alphabetic characters, 'A' through 'Z' and 'a' through 'z'
    digit 	The US-ASCII decimal digit characters, '0' through '9'
    alphanum 	All alpha and digit characters
    unreserved     	All alphanum characters together with those in the string "_-!.~'()*"
    punct 	The characters in the string ",;:$&+="
    reserved 	All punct characters together with those in the string "?/[]@"
    escaped 	Escaped octets, that is, triplets consisting of the percent character ('%') followed by two hexadecimal digits ('0'-'9', 'A'-'F', and 'a'-'f')
    other 	The Unicode characters that are not in the US-ASCII character set, are not control characters (according to the Character.isISOControl method), and are not space characters (according to the Character.isSpaceChar method)  (Deviation from RFC 2396, which is limited to US-ASCII)

The set of all legal URI characters consists of the unreserved, reserved, escaped, and other characters.
Escaped octets, quotation, encoding, and decoding
RFC 2396 allows escaped octets to appear in the user-info, path, query, and fragment components. Escaping serves two purposes in URIs:

    *      To encode non-US-ASCII characters when a URI is required to conform strictly to RFC 2396 by not containing any other characters.
    *      To quote characters that are otherwise illegal in a component. The user-info, path, query, and fragment components differ slightly in terms of which characters are considered legal and illegal.

These purposes are served in this class by three related operations:

    *      A character is encoded by replacing it with the sequence of escaped octets that represent that character in the UTF-8 character set. The Euro currency symbol ('\u20AC'), for example, is encoded as "%E2%82%AC". (Deviation from RFC 2396, which does not specify any particular character set.)
    *      An illegal character is quoted simply by encoding it. The space character, for example, is quoted by replacing it with "%20". UTF-8 contains US-ASCII, hence for US-ASCII characters this transformation has exactly the effect required by RFC 2396.
    *      A sequence of escaped octets is decoded by replacing it with the sequence of characters that it represents in the UTF-8 character set. UTF-8 contains US-ASCII, hence decoding has the effect of de-quoting any quoted US-ASCII characters as well as that of decoding any encoded non-US-ASCII characters. If a decoding error occurs when decoding the escaped octets then the erroneous octets are replaced by '\uFFFD', the Unicode replacement character.

These operations are exposed in the constructors and methods of this class as follows:

    *      The single-argument constructor requires any illegal characters in its argument to be quoted and preserves any escaped octets and other characters that are present.
    *      The multi-argument constructors quote illegal characters as required by the components in which they appear. The percent character ('%') is always quoted by these constructors. Any other characters are preserved.
    *      The getRawUserInfo, getRawPath, getRawQuery, getRawFragment, getRawAuthority, and getRawSchemeSpecificPart methods return the values of their corresponding components in raw form, without interpreting any escaped octets. The strings returned by these methods may contain both escaped octets and other characters, and will not contain any illegal characters.
    *      The getUserInfo, getPath, getQuery, getFragment, getAuthority, and getSchemeSpecificPart methods decode any escaped octets in their corresponding components. The strings returned by these methods may contain both other characters and illegal characters, and will not contain any escaped octets.
    *      The toString method returns a URI string with all necessary quotation but which may contain other characters.
    *      The toASCIIString method returns a fully quoted and encoded URI string that does not contain any other characters.

Identities
For any URI u, it is always the case that

    new URI(u.toString()).equals(u) . 

For any URI u that does not contain redundant syntax such as two slashes before an empty authority (as in file:///tmp/ ) or a colon following a host name but no port (as in http://java.sun.com: ), and that does not encode characters except those that must be quoted, the following identities also hold:

    new URI(u.getScheme(),
            u.getSchemeSpecificPart(),
            u.getFragment())
    .equals(u) 

in all cases,

    new URI(u.getScheme(),
            u.getUserInfo(), u.getAuthority(),
            u.getPath(), u.getQuery(),
            u.getFragment())
    .equals(u) 

if u is hierarchical, and

    new URI(u.getScheme(),
            u.getUserInfo(), u.getHost(), u.getPort(),
            u.getPath(), u.getQuery(),
            u.getFragment())
    .equals(u) 

if u is hierarchical and has either no authority or a server-based authority.
URIs, URLs, and URNs
A URI is a uniform resource identifier while a URL is a uniform resource locator. Hence every URL is a URI, abstractly speaking, but not every URI is a URL. This is because there is another subcategory of URIs, uniform resource names (URNs), which name resources but do not specify how to locate them. The mailto, news, and isbn URIs shown above are examples of URNs.

The conceptual distinction between URIs and URLs is reflected in the differences between this class and the URL class.

An instance of this class represents a URI reference in the syntactic sense defined by RFC 2396. A URI may be either absolute or relative. A URI string is parsed according to the generic syntax without regard to the scheme, if any, that it specifies. No lookup of the host, if any, is performed, and no scheme-dependent stream handler is constructed. Equality, hashing, and comparison are defined strictly in terms of the character content of the instance. In other words, a URI instance is little more than a structured string that supports the syntactic, scheme-independent operations of comparison, normalization, resolution, and relativization.

An instance of the URL class, by contrast, represents the syntactic components of a URL together with some of the information required to access the resource that it describes. A URL must be absolute, that is, it must always specify a scheme. A URL string is parsed according to its scheme. A stream handler is always established for a URL, and in fact it is impossible to create a URL instance for a scheme for which no handler is available. Equality and hashing depend upon both the scheme and the Internet address of the host, if any; comparison is not defined. In other words, a URL is a structured string that supports the syntactic operation of resolution as well as the network I/O operations of looking up the host and opening a connection to the specified resource.

See Also:
    RFC 2279: UTF-8, a transformation format of ISO 10646,
    RFC 2373: IPv6 Addressing Architecture,
    RFC 2396: Uniform Resource Identifiers (URI): Generic Syntax,
    RFC 2732: Format for Literal IPv6 Addresses in URLs,

  License:
    * The contents of this file are released under a dual \license, and
    * you may choose to use it under either the Mozilla Public License
    * 1.1 (MPL 1.1, available from http://www.mozilla.org/MPL/MPL-1.1.html)
    * or the GNU Lesser General Public License 2.1 (LGPL 2.1, available from
    * http://www.opensource.org/licenses/lgpl-license.php).
    * Software distributed under the License is distributed on an "AS
    * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
    * implied. See the License for the specific language governing
    * rights and limitations under the \license.
    * The Original Code is $RCSfile: uMeURI.pas,v $.
    * The Initial Developers of the Original Code are Indy Project.
    * Portions created by Chad Z. Hower and the Indy Pit Crew. is Copyright (C) 1993-2004
    * Portions created by Riceball LEE is Copyright (C) 2006-2008
    * All rights reserved.

    * Contributor(s):
      Chad Z. Hower and the Indy Pit Crew
      ARybin
      czhower
      SPerry
      GGrieve
      BGooijen
      SGrobety
      JPMugaas
      Peter Mee
      Doychin Bondzhev
}
{ Chinese:
表示一个统一资源标识符 (URI) 引用。

除了以下提到的一些细微不同之处外，此类的实例代表一个 URI 引用，这在以下文档中定义：RFC 2396: Uniform Resource Identifiers (URI):Generic Syntax；在此文件中对其内容又进行了修正：RFC 2732:Format for Literal IPv6 Addresses in URLs。字面值 IPv6 地址格式还支持 scope_ids。scope_ids 的语法和用法在此处描述。此类提供了用于从其组成部分或通过解析其字符串形式创建 URI 实例的构造方法、用于访问实例的各个不同组成部分的方法，以及用于对 URI 实例进行规范化、解析和相对化的方法。此类的实例不可变。
URI 语法和组成部分
在最高级别上，字符串形式的 URI 引用（以下简写为 "URI"）语法如下

    [scheme:]scheme-specific-part[#fragment] 

其中，方括号 [...] 用于描述可选组成部分，字符 : 和 # 代表它们自身。

绝对 URI 指定了方案 (scheme)；非绝对的 URI 称为相对 URI。URI 还可以根据其是否为不透明的 或分层的 进行分类。

不透明 URI 为绝对 URI，其特定于方案的部分不是以斜线字符 ('/') 开始。不透明 URI 无法进行进一步解析。下面是不透明 URI 的一些示例：

    mailto:java-net@java.sun.com	
    news:comp.lang.java	
    urn:isbn:096139210x

分层 URI 或者为绝对 URI（其特定于方案的部分以斜线字符开始），或者为相对 URI，即不指定方案的 URI。下面是分层 URI 的一些示例：

    http://java.sun.com/j2se/1.3/
    docs/guide/collections/designfaq.html#28
    ../../../demo/jfc/SwingSet2/src/SwingSet2.java
    file:///~/calendar 

分层 URI 还要按照下面的语法进行进一步的解析

    [scheme:][//authority][path][?query][#fragment] 

其中，:、/、? 和 # 代表它们自身。分层 URI 的特定于方案的部分包含方案和片段部分之间的字符。

分层 URI 的授权组成部分（如果指定）为基于服务器的 或基于注册表的。基于服务器的授权按照如下众所周知的语法进行解析：

    [user-info@]host[:port] 

其中，字符 @ 和 : 代表它们自身。几乎当前使用的所有 URI 方案都是基于服务器的。不能采用这种方式解析的授权组成部分被视为基于注册表的。

如果分层 URI 的路径组成部分以斜线字符 ('/') 开始，则称此 URI 本身为绝对的；否则它为相对的。分层 URI 或者为绝对的，或者指定了授权的路径，它始终为绝对的。

如上所述，URI 实例具有以下九个组成部分：

    组成部分	类型
    方案	String
    特定于方案的部分	String
    授权	String
    用户信息	String
    主机	String
    端口	int
    路径	String
    查询	String
    片段	String

在给定实例中，任何特殊组成部分都或者为未定义的，或者为已定义的，并且有不同的值。未定义的字符串组成部分由 null 表示，未定义的整数组成部分由 -1 表示。已定义的字符串组成部分的值可以为空字符串；这与未定义的组成部分不等效。

实例中特定的组成部分是已定义的还是未定义的取决于所代表的 URI 类型。绝对 URI 具有方案组成部分。不透明的 URI 具有一个方案、一个特定于方案的部分，以及可能会有一个片段，但是没有其他组成部分。分层 URI 总是有一个路径（尽管可能为空）和一个特定于方案的部分（它至少包含一个路径），还可以包含任何其他组成部分。如果有授权组成部分且它又是基于服务器的，则主机组成部分将被定义，也有可能定义用户信息和端口组成部分。
针对 URI 实例的运算
此类支持的主要运算有规范化、解析 和相对化 运算。

规范化 是将分层 URI 的路径组成部分中的不必要的 "." 和 ".." 部分移除的过程。每个 "." 部分都将被移除。".." 部分也被移除，除非它前面有一个非 ".." 部分。规范化对不透明 URI 不产生任何效果。

解析 是根据另一个基本 URI 解析某个 URI 的过程。得到的 URI 由两个 URI 组成部分构造，构造方式由 RFC 2396 指定，从基本 URI 取出原始 URI 中未指定的组成部分。对于分层 URI，原始的路径根据基本路径进行解析，然后进行规范化。例如，解析以下 URI

    docs/guide/collections/designfaq.html#28          (1) 

根据基本 URI http://java.sun.com/j2se/1.3/ 解析，结果为 URI

    http://java.sun.com/j2se/1.3/docs/guide/collections/designfaq.html#28 

解析相对 URI

    ../../../demo/jfc/SwingSet2/src/SwingSet2.java    (2) 

根据此结果应生成

    http://java.sun.com/j2se/1.3/demo/jfc/SwingSet2/src/SwingSet2.java 

支持对绝对和相对 URI，以及分层 URI 的绝对和相对路径的解析。根据任何其他 URI 对 URI file:///~calendar 进行解析只能生成原始的 URI，因为它是绝对路径。根据相对基础 URI (1) 解析相对 URI (2) 将生成规范的但依然是相对的 URI

    demo/jfc/SwingSet2/src/SwingSet2.java 

最后，相对化 是解析的逆过程：对于任何两个规范的 URI u 和 v，

    u.relativize(u.resolve(v)).equals(v)  和
    u.resolve(u.relativize(v)).equals(v)。

此运算在下面的场合非常有用：构造一个包含 URI 的文档，该 URI 必须尽可能是根据文档的基本 URI 建立的相对 URI。例如，相对化 URI

    http://java.sun.com/j2se/1.3/docs/guide/index.html 

根据基本 URI

    http://java.sun.com/j2se/1.3 

生成了相对 URI docs/guide/index.html。
字符分类
RFC 2396 精确指出 URI 引用中的各个不同组成部分允许使用的字符。以下分类大部分取自该规范，这些约束的用法描述如下：

    alpha 	US-ASCII 字母字符，'A' 到 'Z' 以及 'a' 到 'z'
    digit 	US-ASCII 十进制数字符，'0' 到 '9'
    alphanum 	所有 alpha 和 digit 字符
    unreserved     	所有 alphanum 字符及字符串 "_-!.~'()*" 中包含的字符
    punct 	字符串 ",;:$&+=" 中包含的字符
    reserved 	所有 punct 字符及字符串 "?/[]@" 中包含的字符
    escaped 	转义八位组，即三部分组合：百分号 ('%') 后跟两个十六进制数（'0'-'9'、'A'-'F' 和 'a'-'f'）
    other 	未包含在 US-ASCII 字符集中的 Unicode 字符不是控制字符（根据 Character.isISOControl 方法），并且不是空格字符（根据 Character.isSpaceChar 方法）（与 RFC 2396 有些出入，RFC 2396 限制为 US-ASCII）

全部合法 URI 字符集包含 unreserved、reserved、escaped 和 other 字符。
转义八位组、引用、编码和解码
RFC 2396 允许用户信息、路径、查询和片段组成部分中包含转义八位组。转义在 URI 中实现两个目的：

    *      当要求 URI 不能包含任何 other 字符以严格遵守 RFC 2396 时，需要对非 US-ASCII 字符进行编码。
    *      要引用 组成部分中的非法字符。用户信息、路径、查询和片段组成部分在判断哪些字符合法哪些字符非法上稍有不同。

在此类中由三个相关的运算实现了这两个目的：

    *      字符的编码 方式是，用代表该字符在 UTF-8 字符集中的字符的转义八位组序列取代该字符。例如，欧元符号 ('\u20AC') 编码后为 "%E2%82%AC"。（与 RFC 2396 有些出入，RFC 2396 未指定任何特殊字符集）。
    *      非法字符通过简单地对它进行编码来引用。例如，空格字符，用 "%20" 取代它来进行引用。UTF-8 包含 US-ASCII，因此对于 US-ASCII 字符，此转换具有的效果与 RFC 2396 的要求相同。
    *      对转义八位组序列进行解码 的方法是，用它所代表的 UTF-8 字符集中的字符的序列将它取代。UTF-8 包含 US-ASCII，因此解码具有对引用的任何 US-ASCII 字符取消引用的效果，以及对任何编码的非 US-ASCII 字符进行解码的效果。如果在对转义八位组进行解码时出现解码错误，则出错的八位组用 Unicode 替换字符 '\uFFFD' 取代。

这些运算在此类的构造方法和方法中公开，如下所示：

    *      单参数构造方法要求对参数中的任何非法字符都必须引用，并保留出现的任何转义八位组和 other 字符。
    *      多参数构造方法根据其中出现的组成部分的需要对非法字符进行引用。百分号字符 ('%') 始终通过这些构造方法引用。任何 other 字符都将被保留。
    *      getRawUserInfo、getRawPath、getRawQuery、getRawFragment、getRawAuthority 和 getRawSchemeSpecificPart 方法以原始形式返回它们的相应组成部分的值，不解释任何转义八位组。由这些方法返回的字符串有可能包含转义八位组和 other 字符，但不包含任何非法字符。
    *      getUserInfo、getPath、getQuery、getFragment、getAuthority 和 getSchemeSpecificPart 方法解码相应的组成部分中的任何转义八位组。由这些方法返回的字符串有可能包含 other 字符和非法字符，但不包含任何转义八位组。
    *      toString 返回带所有必要引用的 URI 字符串，但它可能包含 other 字符。
    *      toASCIIString 方法返回不包含任何 other 字符的、完全引用的和经过编码的 URI 字符串。

标识
对于任何 URI u，下面的标识有效

    new URI(u.toString()).equals(u) . 

对于不包含冗余语法的任何 URI u，比如在一个空授权前面有两根斜线（如 file:///tmp/）和主机名后面跟一个冒号但没有端口（如 http://java.sun.com:），以及除必须引用的字符之外不对字符编码的情况，下面的标识也有效：

    new URI(u.getScheme()、
            u.getSchemeSpecificPart()、
            u.getFragment())
    .equals(u) 

在所有情况下，以下标识有效

    new URI(u.getScheme()、
            u.getUserInfo()、 u.getAuthority()、
            u.getPath()、 u.getQuery()、
            u.getFragment())
    .equals(u) 

如果 u 为分层的，则以下标识有效

    new URI(u.getScheme()、
            u.getUserInfo()、 u.getHost()、 u.getPort()、
            u.getPath()、 u.getQuery()、
            u.getFragment())
    .equals(u) 

如果 u 为分层的并且没有授权或没有基于服务器的授权。
URI、URL 和 URN
URI 是统一资源标识符，而 URL 是统一资源定位符。因此，笼统地说，每个 URL 都是 URI，但不一定每个 URI 都是 URL。这是因为 URI 还包括一个子类，即统一资源名称 (URN)，它命名资源但不指定如何定位资源。上面的 mailto、news 和 isbn URI 都是 URN 的示例。

URI 和 URL 概念上的不同反映在此类和 URL 类的不同中。

此类的实例代表由 RFC 2396 定义的语法意义上的一个 URI 引用。URI 可以是绝对的，也可以是相对的。对 URI 字符串按照一般语法进行解析，不考虑它所指定的方案（如果有）不对主机（如果有）执行查找，也不构造依赖于方案的流处理程序。相等性、哈希计算以及比较都严格地根据实例的字符内容进行定义。换句话说，一个 URI 实例和一个支持语法意义上的、依赖于方案的比较、规范化、解析和相对化计算的结构化字符串差不多。

作为对照，URL 类的实例代表了 URL 的语法组成部分以及访问它描述的资源所需的信息。URL 必须是绝对的，即它必须始终指定一个方案。URL 字符串按照其方案进行解析。通常会为 URL 建立一个流处理程序，实际上无法为未提供处理程序的方案创建一个 URL 实例。相等性和哈希计算依赖于方案和主机的 Internet 地址（如果有）；没有定义比较。换句话说，URL 是一个结构化字符串，它支持解析的语法运算以及查找主机和打开到指定资源的连接之类的网络 I/O 操作。
}

unit uMeURI;

interface

{$I MeSetting.inc}

uses
  SysUtils
  , uMeObject
  ;

type
  TMeURIOptionalFields = (ofAuthInfo, ofBookmark);
  TMeURIOptionalFieldsSet = set of TMeURIOptionalFields;
  TMeIpVersion = (ivIPv4, ivIPv6);

  PMeURI = ^ TMeURI;
  TMeURI = object(TMeDynamicObject)
  protected
    FDocument: string;
    FProtocol: string;
    FURI: String;
    FPort: string;
    Fpath: string;
    FHost: string;
    FBookmark: string;
    FUserName: string;
    FPassword: string;
    FParams: string;
    FIPVersion: TMeIpVersion;

    procedure SetURI(const Value: String);
    function GetURI: String;
  public
    destructor Destroy; virtual; { override }
    function GetFullURI(const AOptionalFields: TMeURIOptionalFieldsSet = [ofAuthInfo, ofBookmark]): String;
    function GetPathAndParams: String;
    { Summary: Normalize the directory delimiters to follow the UNIX syntax }
    class procedure NormalizePath(var APath: string);
    class function URLDecode(ASrc: string): string;
    class function URLEncode(const ASrc: string): string;
    class function ParamsEncode(const ASrc: string): string;
    class function PathEncode(const ASrc: string): string;
    { Summary: the Fragment part. }
    property Bookmark : string read FBookmark write FBookmark;
    property Document: string read FDocument write FDocument;
    property Host: string read FHost write FHost;
    property Password: string read FPassword write FPassword;
    property Path: string read FPath write FPath;
    { Sumary: the Query part. }
    property Params: string read FParams write FParams;
    property Port: string read FPort write FPort;
    { Summary: the Scheme part}
    { 
    Components of all URIs: [<scheme>:]<scheme-specific-part>[#<fragment>]
    null ==> relative URI
    }
    property Protocol: string read FProtocol write FProtocol;
    property URI: string read GetURI write SetURI;
    property Username: string read FUserName write FUserName;
    property IPVersion : TMeIpVersion read FIPVersion write FIPVersion;
  end;

resourcestring
  RSURINoProto                 = 'Protocol field is empty';
  RSURINoHost                  = 'Host field is empty';

implementation

uses
  uMeSystem, uMeStrUtils;

{ TMeURI }
destructor TMeURI.Destroy;
begin
  FURI := '';
  FProtocol := '';
  FParams := '';
  FHost := '';
  FPort := '';
  FPassword := '';
  FBookmark := '';
  FPath := '';
  FUserName := '';
  FDocument := '';
  inherited;
end;

class procedure TMeURI.NormalizePath(var APath: string);
var
  i: Integer;
begin
  i := 1;
  while i <= Length(APath) do begin
    if IsLeadChar(APath[i]) then begin
      inc(i, 2)
    end else if APath[i] = '\' then begin    {Do not Localize}
      APath[i] := '/';    {Do not Localize}
      inc(i, 1);
    end else begin
      inc(i, 1);
    end;
  end;
end;

procedure TMeURI.SetURI(const Value: String);
var
  LBuffer: string;
  LTokenPos: Integer;
  LURI: string;
begin
  FURI := Value;
  NormalizePath(FURI);
  LURI := FURI;
  FHost := '';    {Do not Localize}
  FProtocol := '';    {Do not Localize}
  FPath := '';    {Do not Localize}
  FDocument := '';    {Do not Localize}
  FPort := '';    {Do not Localize}
  FBookmark := '';    {Do not Localize}
  FUsername := '';    {Do not Localize}
  FPassword := '';    {Do not Localize}
  FParams := '';  {Do not localise}  //Peter Mee
  FIPVersion := ivIPv4;

  // Parse the # bookmark from the document
  LTokenPos := RPos('#', LURI);    {Do not Localize}
  FBookmark := LURI;
  LURI := StrFetch(FBookmark, '#');    {Do not Localize}

  LTokenPos := AnsiPos(':', LURI);    {Do not Localize}
  if LTokenPos > 0 then begin
    // absolute URI
    // What to do when data don't match configuration ??    {Do not Localize}
    // Get the protocol
    FProtocol := Copy(LURI, 1, LTokenPos  - 1);
    Delete(LURI, 1, LTokenPos);
    {if (Length(LURI) >=2) and (LURI[1] = '/') and (LURI[2] = '/') then
    begin
      //it's a URL
      Delete(LURI, 1, 2);
    end;
    //else //it is a URN}

    // separate the path from the parameters
    LTokenPos := AnsiPos('?', LURI);    {Do not Localize}
    if LTokenPos = 0 then begin
      LTokenPos := AnsiPos('=', LURI);    {Do not Localize}
    end;
    if LTokenPos > 0 then begin
      FParams := Copy(LURI, LTokenPos + 1, MaxInt);
      LURI := Copy(LURI, 1, LTokenPos - 1);
    end;
    // Get the user name, password, host and the port number
    LBuffer := LURI;
    while (Length(LBuffer) > 0) and (LBuffer[1] = '/') do
      Delete(LBuffer, 1, 1);
    LTokenPos := RPos('/', LBuffer);
    if LTokenPos > 0 then
    begin
      LBuffer := Copy(LBuffer, 1, LTokenPos-1);
      LTokenPos := RPos('/', LURI);
      LURI := Copy(LURI, LTokenPos+1, MaxInt);
      LBuffer := StrFetch(LBuffer, '/', False);    {Do not Localize}
    end;
    //LBuffer := StrFetch(LURI, '/', True);    {Do not Localize}
    // Get username and password
    LTokenPos := AnsiPos('@', LBuffer);    {Do not Localize}
    if LTokenPos > 0 then begin
      FUsername := Copy(LBuffer, 1, LTokenPos  - 1);
      Delete(LBuffer, 1, LTokenPos);
      LTokenPos := AnsiPos(':', FUsername);    {Do not Localize}
      if LTokenPos > 0 then begin
        FPassword := Copy(FUsername, LTokenPos+1, MaxInt);
        FUsername := Copy(FUsername, 1, LTokenPos-1);
      end;
      // Ignore cases where there is only password (http://:password@host/pat/doc)
      if Length(FUserName) = 0 then begin
        FPassword := '';    {Do not Localize}
      end;
    end;
    // Get the host and the port number
    if (AnsiPos('[', LBuffer) > 0) and (AnsiPos(']', LBuffer) > AnsiPos('[', LBuffer)) then begin {Do not Localize}
      //This is for IPv6 Hosts
      FHost := StrFetch(LBuffer, ']'); {Do not Localize}
      StrFetch(FHost, '['); {Do not Localize}
      StrFetch(LBuffer, ':'); {Do not Localize}
      FIPVersion := ivIPv6;
    end else begin
      FHost := StrFetch(LBuffer, ':', True);    {Do not Localize}
    end;
    FPort := LBuffer;
    // Get the path
    LTokenPos := RPos('/', LURI);
    if LTokenPos > 0 then begin
      FPath := '/' + Copy(LURI, 1, LTokenPos);    {Do not Localize}
      Delete(LURI, 1, LTokenPos);
    end else begin
      FPath := '/';    {Do not Localize}
    end;
  end else begin
    // received an absolute path, not an URI
    LTokenPos := AnsiPos('?', LURI);    {Do not Localize}
    if LTokenPos = 0 then begin
      LTokenPos := AnsiPos('=', LURI);    {Do not Localize}
    end;
    if LTokenPos > 0 then begin // The case when there is parameters after the document name
      FParams := Copy(LURI, LTokenPos + 1, MaxInt);
      LURI := Copy(LURI, 1, LTokenPos - 1);
    end;
    // Get the path
    LTokenPos := RPos('/', LURI);    {Do not Localize}
    if LTokenPos > 0 then begin
      FPath := Copy(LURI, 1, LTokenPos);
      Delete(LURI, 1, LTokenPos);
    end;
  end;
  // Get the document
  FDocument := LURI;
end;

function TMeURI.GetURI: String;
begin
  FURI := GetFullURI;
  // Result must contain only the proto://host/path/document
  // If you need the full URI then you have to call GetFullURI
  Result := GetFullURI([]);
end;

class function TMeURI.URLDecode(ASrc: string): string;
var
  i: Integer;
  ESC: string[4];
  CharCode: Integer;
begin
  Result := '';    {Do not Localize}
  // S.G. 27/11/2002: Spaces is NOT to be encoded as "+".
  // S.G. 27/11/2002: "+" is a field separator in query parameter, space is...
  // S.G. 27/11/2002: well, a space
  // ASrc := StringReplace(ASrc, '+', ' ', [rfReplaceAll]);  {do not localize}
  i := 1;
  while i <= Length(ASrc) do begin
    if ASrc[i] <> '%' then begin  {do not localize}
      Result := Result + ASrc[i]; // Copy the char
      Inc(i); // Then skip it
    end else begin
      Inc(i); // skip the % char
      if not CharIsInSet(ASrc, i, 'uU') then begin  {do not localize}
        // simple ESC char
        ESC := Copy(ASrc, i, 2); // Copy the escape code
        Inc(i, 2); // Then skip it.
        try
          CharCode := StrToInt('$' + ESC);  {do not localize}
          Result := Result + Char(CharCode);
        except end;
      end else
      begin
        // unicode ESC code

        // RLebeau 5/10/2006: under Win32, the character will end
        // up as '?' in the Result when converted from Unicode to Ansi,
        // but at least the URL will be parsed properly
        
        ESC := Copy(ASrc, i+1, 4); // Copy the escape code
        Inc(i, 5); // Then skip it.
        try
          CharCode := StrToInt('$' + ESC);  {do not localize}
          Result := Result + WideChar(CharCode);
        except end;
      end;
    end;
  end;
end;

class function TMeURI.ParamsEncode(const ASrc: string): string;
var
  i: Integer;
const
  UnsafeChars = '*#%<> []';  {do not localize}
begin
  Result := '';    {Do not Localize}
  for i := 1 to Length(ASrc) do
  begin
    // S.G. 27/11/2002: Changed the parameter encoding: Even in parameters, a space
    // S.G. 27/11/2002: is much more likely to be meaning "space" than "this is
    // S.G. 27/11/2002: a new parameter"
    // S.G. 27/11/2002: ref: Message-ID: <3de30169@newsgroups.borland.com> borland.public.delphi.internet.winsock
    // S.G. 27/11/2002: Most low-ascii is actually Ok in parameters encoding.
    if CharIsInSet(ASrc, i, UnsafeChars) or (not CharIsInSet(ASrc, i, CharRange(#33,#128))) then begin {do not localize}
      Result := Result + '%' + IntToHex(Ord(ASrc[i]), 2);  {do not localize}
    end else begin
      Result := Result + ASrc[i];
    end;
  end;
end;

class function TMeURI.PathEncode(const ASrc: string): string;
const
  UnsafeChars = '*#%<>+ []';  {do not localize}
var
  i: Integer;
begin
  Result := '';    {Do not Localize}
  for i := 1 to Length(ASrc) do begin
    if CharIsInSet(ASrc, i, UnsafeChars) or (not CharIsInSet(ASrc, i, CharRange(#32, #127))) then begin
      Result := Result + '%' + IntToHex(Ord(ASrc[i]), 2);  {do not localize}
    end else begin
      Result := Result + ASrc[i];
    end;
  end;
end;

class function TMeURI.URLEncode(const ASrc: string): string;
begin
  with New(PMeURI, Create)^ do 
  try
    URI := aSrc;
    Path := PathEncode(Path);
    Document := PathEncode(Document);
    Params := ParamsEncode(Params);
    Result := URI;
  finally 
    Free; 
  end;
end;

function TMeURI.GetFullURI(const AOptionalFields: TMeURIOptionalFieldsSet): String;
var
  LURI: String;
begin
  if FProtocol = '' then begin
    raise EMeError.Create(RSURINoProto);
  end;

  if FHost = '' then begin
    raise EMeError.Create(RSURINoHost);
  end;

  LURI := FProtocol + '://';    {Do not Localize}

  if (FUserName <> '') and (ofAuthInfo in AOptionalFields) then begin
    LURI := LURI + FUserName;
    if FPassword <> '' then begin
      LURI := LURI + ':' + FPassword;    {Do not Localize}
    end;
    LURI := LURI + '@';    {Do not Localize}
  end;

  LURI := LURI + FHost;
  if FPort <> '' then begin
    case PosInStrArray(FProtocol, ['HTTP', 'HTTPS', 'FTP'], False) of {Do not Localize}
      0:
        begin
          if FPort <> '80' then begin
            LURI := LURI + ':' + FPort;    {Do not Localize}
          end;
        end;
      1:
        begin
          if FPort <> '443' then begin
            LURI := LURI + ':' + FPort;    {Do not Localize}
          end;
        end;
      2:
        begin
          if FPort <> '21' then begin
            LURI := LURI + ':' + FPort;    {Do not Localize}
          end;
        end;
      else
        begin
          LURI := LURI + ':' + FPort;    {Do not Localize}
        end;
    end;
  end;

  LURI := LURI + GetPathAndParams;

  if (FBookmark <> '') and (ofBookmark in AOptionalFields) then begin
    LURI := LURI + '#' + FBookmark;    {Do not Localize}
  end;

  Result := LURI;
end;

function TMeURI.GetPathAndParams: String;
begin
  Result := FPath + FDocument;
  if FParams <> '' then begin
    Result := Result + '?' + FParams; {Do not Localize}
  end;
end;

end.
