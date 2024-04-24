"use strict";
const body = document.body;
let screen_size = [innerWidth, innerHeight],
  b_searc = [],
  b_sites = [],
  scroll_route_delayed,
  openw = ["_blank", "_blank", "_blank"],
  mousewheel = "mousewheel";
firefox && (mousewheel = "DOMMouseScroll");
const serial_piny = {
  a: "啊阿锕",
  ai: "埃挨哎唉哀皑癌蔼矮艾碍爱隘诶捱嗳嗌嫒瑷暧砹锿霭",
  an: "鞍氨安俺按暗岸胺案谙埯揞犴庵桉铵鹌顸黯",
  ang: "肮昂盎",
  ao: "凹敖熬翱袄傲奥懊澳坳拗嗷噢岙廒遨媪骜聱螯鏊鳌鏖",
  ba: "芭捌扒叭吧笆八疤巴拔跋靶把耙坝霸罢爸茇菝萆捭岜灞杷钯粑鲅魃",
  bai: "白柏百摆佰败拜稗薜掰鞴",
  ban: "斑班搬扳般颁板版扮拌伴瓣半办绊阪坂豳钣瘢癍舨",
  bang: "邦帮梆榜膀绑棒磅蚌镑傍谤蒡螃",
  bao: "苞胞包褒雹保堡饱宝抱报暴豹鲍爆勹葆宀孢煲鸨褓趵龅",
  bo: "剥薄玻菠播拨钵波博勃搏铂箔伯帛舶脖膊渤泊驳亳蕃啵饽檗擘礴钹鹁簸跛",
  bei: "杯碑悲卑北辈背贝钡倍狈备惫焙被孛陂邶埤蓓呗怫悖碚鹎褙鐾",
  ben: "奔苯本笨畚坌锛",
  beng: "崩绷甭泵蹦迸唪嘣甏",
  bi: "逼鼻比鄙笔彼碧蓖蔽毕毙毖币庇痹闭敝弊必辟壁臂避陛匕仳俾芘荜荸吡哔狴庳愎滗濞弼妣婢嬖璧贲畀铋秕裨筚箅篦舭襞跸髀",
  bian: "鞭边编贬扁便变卞辨辩辫遍匾弁苄忭汴缏煸砭碥稹窆蝙笾鳊",
  biao: "标彪膘表婊骠飑飙飚灬镖镳瘭裱鳔",
  bie: "鳖憋别瘪蹩鳘",
  bin: "彬斌濒滨宾摈傧浜缤玢殡膑镔髌鬓",
  bing: "兵冰柄丙秉饼炳病并禀邴摒绠枋槟燹",
  bu: "捕卜哺补埠不布步簿部怖拊卟逋瓿晡钚醭",
  ca: "擦嚓礤",
  cai: "猜裁材才财睬踩采彩菜蔡",
  can: "餐参蚕残惭惨灿骖璨粲黪",
  cang: "苍舱仓沧藏伧",
  cao: "操糙槽曹草艹嘈漕螬艚",
  ce: "厕策侧册测刂帻恻",
  ceng: "层蹭噌",
  cha: "插叉茬茶查碴搽察岔差诧猹馇汊姹杈楂槎檫钗锸镲衩",
  chai: "拆柴豺侪茈瘥虿龇",
  chan: "搀掺蝉馋谗缠铲产阐颤冁谄谶蒇廛忏潺澶孱羼婵嬗骣觇禅镡裣蟾躔",
  chang: "昌猖场尝常长偿肠厂敞畅唱倡伥鬯苌菖徜怅惝阊娼嫦昶氅鲳",
  chao: "超抄钞朝嘲潮巢吵炒怊绉晁耖",
  che: "车扯撤掣彻澈坼屮砗",
  chen: "郴臣辰尘晨忱沉陈趁衬称谌抻嗔宸琛榇肜胂碜龀",
  cheng: "撑城橙成呈乘程惩澄诚承逞骋秤埕嵊徵浈枨柽樘晟塍瞠铖裎蛏酲",
  chi: "吃痴持匙池迟弛驰耻齿侈尺赤翅斥炽傺墀芪茌搋叱哧啻嗤彳饬沲媸敕胝眙眵鸱瘛褫蚩螭笞篪豉踅踟魑",
  chong: "充冲虫崇宠茺忡憧铳艟",
  chou: "抽酬畴踌稠愁筹仇绸瞅丑俦圳帱惆溴妯瘳雠鲋",
  chu: "臭初出橱厨躇锄雏滁除楚础储矗搐触处亍刍憷绌杵楮樗蜍蹰黜",
  chuan: "揣川穿椽传船喘串掾舛惴遄巛氚钏镩舡",
  chuang: "疮窗幢床闯创怆",
  chui: "吹炊捶锤垂陲棰槌",
  chun: "春椿醇唇淳纯蠢促莼沌肫朐鹑蝽",
  chuo: "戳绰蔟辶辍镞踔龊",
  ci: "疵茨磁雌辞慈瓷词此刺赐次荠呲嵯鹚螅糍趑",
  cong: "聪葱囱匆从丛偬苁淙骢琮璁枞",
  cu: "凑粗醋簇猝殂蹙",
  cuan: "蹿篡窜汆撺昕爨",
  cui: "摧崔催脆瘁粹淬翠萃悴璀榱隹",
  cun: "村存寸磋忖皴",
  cuo: "撮搓措挫错厝脞锉矬痤鹾蹉躜",
  da: "搭达答瘩打大耷哒嗒怛妲疸褡笪靼鞑",
  dai: "呆歹傣戴带殆代贷袋待逮怠埭甙呔岱迨逯骀绐玳黛",
  dan: "耽担丹单郸掸胆旦氮但惮淡诞弹蛋亻儋卩萏啖澹檐殚赕眈瘅聃箪",
  dang: "当挡党荡档谠凼菪宕砀铛裆",
  dao: "刀捣蹈倒岛祷导到稻悼道盗叨啁忉洮氘焘忑纛",
  de: "德得的锝",
  deng: "蹬灯登等瞪凳邓噔嶝戥磴镫簦",
  di: "堤低滴迪敌笛狄涤翟嫡抵底地蒂第帝弟递缔氐籴诋谛邸坻莜荻嘀娣柢棣觌砥碲睇镝羝骶",
  dian: "颠掂滇碘点典靛垫电佃甸店惦奠淀殿丶阽坫埝巅玷癜癫簟踮",
  diao: "碉叼雕凋刁掉吊钓调轺铞蜩粜貂",
  die: "跌爹碟蝶迭谍叠佚垤堞揲喋渫轶牒瓞褶耋蹀鲽鳎",
  ding: "丁盯叮钉顶鼎锭定订丢仃啶玎腚碇町铤疔耵酊",
  dong: "东冬董懂动栋侗恫冻洞垌咚岽峒夂氡胨胴硐鸫",
  dou: "兜抖斗陡豆逗痘蔸钭窦窬蚪篼酡",
  du: "都督毒犊独读堵睹赌杜镀肚度渡妒芏嘟渎椟橐牍蠹笃髑黩",
  duan: "端短锻段断缎彖椴煅簖",
  dui: "堆兑队对怼憝碓",
  dun: "墩吨蹲敦顿囤钝盾遁炖砘礅盹镦趸",
  duo: "掇哆多夺垛躲朵跺舵剁惰堕咄哚缍柁铎裰踱",
  e: "蛾峨鹅俄额讹娥恶厄扼遏鄂饿噩谔垩垭苊莪萼呃愕屙婀轭曷腭硪锇锷鹗颚鳄",
  en: "恩蒽摁唔嗯",
  er: "而儿耳尔饵洱二贰迩珥铒鸸鲕",
  fa: "发罚筏伐乏阀法珐垡砝",
  fan: "藩帆番翻樊矾钒繁凡烦反返范贩犯饭泛蘩幡犭梵攵燔畈蹯",
  fang: "坊芳方肪房防妨仿访纺放匚邡彷钫舫鲂",
  fei: "菲非啡飞肥匪诽吠肺废沸费芾狒悱淝妃绋绯榧腓斐扉祓砩镄痱蜚篚翡霏鲱",
  fen: "芬酚吩氛分纷坟焚汾粉奋份忿愤粪偾瀵棼愍鲼鼢",
  feng: "丰封枫蜂峰锋风疯烽逢冯缝讽奉凤俸酆葑沣砜",
  fu: "佛否夫敷肤孵扶拂辐幅氟符伏俘服浮涪福袱弗甫抚辅俯釜斧脯腑府腐赴副覆赋复傅付阜父腹负富讣附妇缚咐匐凫郛芙苻茯莩菔呋幞滏艴孚驸绂桴赙黻黼罘稃馥虍蚨蜉蝠蝮麸趺跗鳆",
  ga: "噶嘎蛤尬呷尕尜旮钆",
  gai: "该改概钙盖溉丐陔垓戤赅胲",
  gan: "干甘杆柑竿肝赶感秆敢赣坩苷尴擀泔淦澉绀橄旰矸疳酐",
  gang: "冈刚钢缸肛纲岗港戆罡颃筻",
  gong: "杠工攻功恭龚供躬公宫弓巩汞拱贡共蕻廾咣珙肱蚣蛩觥",
  gao: "篙皋高膏羔糕搞镐稿告睾诰郜蒿藁缟槔槁杲锆",
  ge: "哥歌搁戈鸽胳疙割革葛格阁隔铬个各鬲仡哿塥嗝纥搿膈硌铪镉袼颌虼舸骼髂",
  gei: "给",
  gen: "根跟亘茛哏艮",
  geng: "耕更庚羹埂耿梗哽赓鲠",
  gou: "钩勾沟苟狗垢构购够佝诟岣遘媾缑觏彀鸲笱篝鞲",
  gu: "辜菇咕箍估沽孤姑鼓古蛊骨谷股故顾固雇嘏诂菰哌崮汩梏轱牯牿胍臌毂瞽罟钴锢瓠鸪鹄痼蛄酤觚鲴骰鹘",
  gua: "刮瓜剐寡挂褂卦诖呱栝鸹",
  guai: "乖拐怪哙",
  guan: "棺关官冠观管馆罐惯灌贯倌莞掼涫盥鹳鳏",
  guang: "光广逛犷桄胱疒",
  gui: "瑰规圭硅归龟闺轨鬼诡癸桂柜跪贵刽匦刿庋宄妫桧炅晷皈簋鲑鳜",
  gun: "辊滚棍丨衮绲磙鲧",
  guo: "锅郭国果裹过馘蠃埚掴呙囗帼崞猓椁虢锞聒蜮蜾蝈",
  ha: "哈",
  hai: "骸孩海氦亥害骇咴嗨颏醢",
  han: "酣憨邯韩含涵寒函喊罕翰撼捍旱憾悍焊汗汉邗菡撖阚瀚晗焓颔蚶鼾",
  hen: "夯痕很狠恨",
  hang: "杭航沆绗珩桁",
  hao: "壕嚎豪毫郝好耗号浩薅嗥嚆濠灏昊皓颢蚝",
  he: "呵喝荷菏核禾和何合盒貉阂河涸赫褐鹤贺诃劾壑藿嗑嗬阖盍蚵翮",
  hei: "嘿黑",
  heng: "哼亨横衡恒訇蘅",
  hong: "轰哄烘虹鸿洪宏弘红黉讧荭薨闳泓",
  hou: "喉侯猴吼厚候后堠後逅瘊篌糇鲎骺",
  hu: "呼乎忽瑚壶葫胡蝴狐糊湖弧虎唬护互沪户冱唿囫岵猢怙惚浒滹琥槲轷觳烀煳戽扈祜鹕鹱笏醐斛",
  hua: "花哗华猾滑画划化话劐浍骅桦铧稞",
  huai: "槐徊怀淮坏还踝",
  huan: "欢环桓缓换患唤痪豢焕涣宦幻郇奂垸擐圜洹浣漶寰逭缳锾鲩鬟",
  huang: "荒慌黄磺蝗簧皇凰惶煌晃幌恍谎隍徨湟潢遑璜肓癀蟥篁鳇",
  hui: "灰挥辉徽恢蛔回毁悔慧卉惠晦贿秽会烩汇讳诲绘诙茴荟蕙哕喙隳洄彗缋珲晖恚虺蟪麾",
  hun: "荤昏婚魂浑混诨馄阍溷缗",
  huo: "豁活伙火获或惑霍货祸攉嚯夥钬锪镬耠蠖",
  ji: "击圾基机畸稽积箕肌饥迹激讥鸡姬绩缉吉极棘辑籍集及急疾汲即嫉级挤几脊己蓟技冀季伎祭剂悸济寄寂计记既忌际妓继纪居丌乩剞佶佴脔墼芨芰萁蒺蕺掎叽咭哜唧岌嵴洎彐屐骥畿玑楫殛戟戢赍觊犄齑矶羁嵇稷瘠瘵虮笈笄暨跻跽霁鲚鲫髻麂",
  jia: "嘉枷夹佳家加荚颊贾甲钾假稼价架驾嫁伽郏拮岬浃迦珈戛胛恝铗镓痂蛱笳袈跏",
  jian: "歼监坚尖笺间煎兼肩艰奸缄茧检柬碱硷拣捡简俭剪减荐槛鉴践贱见键箭件健舰剑饯渐溅涧建僭谏谫菅蒹搛囝湔蹇謇缣枧柙楗戋戬牮犍毽腱睑锏鹣裥笕箴翦趼踺鲣鞯",
  jiang: "僵姜将浆江疆蒋桨奖讲匠酱降茳洚绛缰犟礓耩糨豇",
  jiao: "蕉椒礁焦胶交郊浇骄娇嚼搅铰矫侥脚狡角饺缴绞剿教酵轿较叫佼僬茭挢噍峤徼姣纟敫皎鹪蛟醮跤鲛",
  jie: "窖揭接皆秸街阶截劫节桔杰捷睫竭洁结解姐戒藉芥界借介疥诫届偈讦诘喈嗟獬婕孑桀獒碣锴疖袷颉蚧羯鲒骱髫",
  jin: "巾筋斤金今津襟紧锦仅谨进靳晋禁近烬浸尽卺荩堇噤馑廑妗缙瑾槿赆觐钅锓衿矜",
  jing: "劲荆兢茎睛晶鲸京惊精粳经井警景颈静境敬镜径痉靖竟竞净刭儆阱菁獍憬泾迳弪婧肼胫腈旌",
  jiong: "炯窘冂迥扃",
  jiu: "揪究纠玖韭久灸九酒厩救旧臼舅咎就疚僦啾阄柩桕鹫赳鬏",
  ju: "鞠拘狙疽驹菊局咀矩举沮聚拒据巨具距踞锯俱句惧炬剧倨讵苣苴莒掬遽屦琚枸椐榘榉橘犋飓钜锔窭裾趄醵踽龃雎鞫",
  juan: "捐鹃娟倦眷卷绢鄄狷涓桊蠲锩镌隽",
  jue: "撅攫抉掘倔爵觉决诀绝厥劂谲矍蕨噘崛獗孓珏桷橛爝镢蹶觖",
  jun: "均菌钧军君峻俊竣浚郡骏捃狻皲筠麇",
  ka: "喀咖卡佧咔胩",
  ke: "咯坷苛柯棵磕颗科壳咳可渴克刻客课岢恪溘骒缂珂轲氪瞌钶疴窠蝌髁",
  kai: "开揩楷凯慨剀垲蒈忾恺铠锎",
  kan: "刊堪勘坎砍看侃凵莰莶戡龛瞰",
  kang: "康慷糠扛抗亢炕坑伉闶钪",
  kao: "考拷烤靠尻栲犒铐",
  ken: "肯啃垦恳垠裉颀",
  keng: "吭忐铿",
  kong: "空恐孔控倥崆箜",
  kou: "抠口扣寇芤蔻叩眍筘",
  ku: "枯哭窟苦酷库裤刳堀喾绔骷",
  kua: "夸垮挎跨胯侉",
  kuai: "块筷侩快蒯郐蒉狯脍",
  kuan: "宽款髋",
  kuang: "匡筐狂框矿眶旷况诓诳邝圹夼哐纩贶",
  kui: "亏盔岿窥葵奎魁傀馈愧溃馗匮夔隗揆喹喟悝愦阕逵暌睽聩蝰篑臾跬",
  kun: "坤昆捆困悃阃琨锟醌鲲髡",
  kuo: "括扩廓阔蛞",
  la: "垃拉喇蜡腊辣啦剌摺邋旯砬瘌",
  lai: "莱来赖崃徕涞濑赉睐铼癞籁",
  lan: "蓝婪栏拦篮阑兰澜谰揽览懒缆烂滥啉岚懔漤榄斓罱镧褴",
  lang: "琅榔狼廊郎朗浪莨蒗啷阆锒稂螂",
  lao: "捞劳牢老佬姥酪烙涝唠崂栳铑铹痨醪",
  le: "勒乐肋仂叻嘞泐鳓",
  lei: "雷镭蕾磊累儡垒擂类泪羸诔荽咧漯嫘缧檑耒酹",
  ling: "棱冷拎玲菱零龄铃伶羚凌灵陵岭领另令酃塄苓呤囹泠绫柃棂瓴聆蛉翎鲮",
  leng: "楞愣",
  li: "厘梨犁黎篱狸离漓理李里鲤礼莉荔吏栗丽厉励砾历利傈例俐痢立粒沥隶力璃哩俪俚郦坜苈莅蓠藜捩呖唳喱猁溧澧逦娌嫠骊缡珞枥栎轹戾砺詈罹锂鹂疠疬蛎蜊蠡笠篥粝醴跞雳鲡鳢黧",
  lian: "俩联莲连镰廉怜涟帘敛脸链恋炼练挛蔹奁潋濂娈琏楝殓臁膦裢蠊鲢",
  liang: "粮凉梁粱良两辆量晾亮谅墚椋踉靓魉",
  liao: "撩聊僚疗燎寥辽潦了撂镣廖料蓼尥嘹獠寮缭钌鹩耢",
  lie: "列裂烈劣猎冽埒洌趔躐鬣",
  lin: "琳林磷霖临邻鳞淋凛赁吝蔺嶙廪遴檩辚瞵粼躏麟",
  liu: "溜琉榴硫馏留刘瘤流柳六抡偻蒌泖浏遛骝绺旒熘锍镏鹨鎏",
  long: "龙聋咙笼窿隆垄拢陇弄垅茏泷珑栊胧砻癃",
  lou: "楼娄搂篓漏陋喽嵝镂瘘耧蝼髅",
  lu: "芦卢颅庐炉掳卤虏鲁麓碌露路赂鹿潞禄录陆戮垆摅撸噜泸渌漉璐栌橹轳辂辘氇胪镥鸬鹭簏舻鲈",
  lv: "驴吕铝侣旅履屡缕虑氯律率滤绿捋闾榈膂稆褛",
  luan: "峦孪滦卵乱栾鸾銮",
  lue: "掠略锊",
  lun: "轮伦仑沦纶论囵",
  luo: "萝螺罗逻锣箩骡裸落洛骆络倮荦摞猡泺椤脶镙瘰雒",
  ma: "妈麻玛码蚂马骂嘛吗唛犸嬷杩麽",
  mai: "埋买麦卖迈脉劢荬咪霾",
  man: "瞒馒蛮满蔓曼慢漫谩墁幔缦熳镘颟螨鳗鞔",
  mang: "芒茫盲忙莽邙漭朦硭蟒",
  meng: "氓萌蒙檬盟锰猛梦孟勐甍瞢懵礞虻蜢蠓艋艨黾",
  miao: "猫苗描瞄藐秒渺庙妙喵邈缈缪杪淼眇鹋蜱",
  mao: "茅锚毛矛铆卯茂冒帽貌贸侔袤勖茆峁瑁昴牦耄旄懋瞀蛑蝥蟊髦",
  me: "么",
  mei: "玫枚梅酶霉煤没眉媒镁每美昧寐妹媚坶莓嵋猸浼湄楣镅鹛袂魅",
  men: "门闷们扪玟焖懑钔",
  mi: "眯醚靡糜迷谜弥米秘觅泌蜜密幂芈冖谧蘼嘧猕獯汨宓弭脒敉糸縻麋",
  mian: "棉眠绵冕免勉娩缅面沔湎腼眄",
  mie: "蔑灭咩蠛篾",
  min: "民抿皿敏悯闽苠岷闵泯珉",
  ming: "明螟鸣铭名命冥茗溟暝瞑酩",
  miu: "谬",
  mo: "摸摹蘑模膜磨摩魔抹末莫墨默沫漠寞陌谟茉蓦馍嫫镆秣瘼耱蟆貊貘",
  mou: "谋牟某厶哞婺眸鍪",
  mu: "拇牡亩姆母墓暮幕募慕木目睦牧穆仫苜呒沐毪钼",
  na: "拿哪呐钠那娜纳内捺肭镎衲箬",
  nai: "氖乃奶耐奈鼐艿萘柰",
  nan: "南男难囊喃囡楠腩蝻赧",
  nao: "挠脑恼闹孬垴猱瑙硇铙蛲",
  ne: "淖呢讷",
  nei: "馁",
  nen: "嫩能枘恁",
  ni: "妮霓倪泥尼拟你匿腻逆溺伲坭猊怩滠昵旎祢慝睨铌鲵",
  nian: "蔫拈年碾撵捻念廿辇黏鲇鲶",
  niang: "娘酿",
  niao: "鸟尿茑嬲脲袅",
  nie: "捏聂孽啮镊镍涅乜陧蘖嗫肀颞臬蹑",
  nin: "您柠",
  ning: "狞凝宁拧泞佞蓥咛甯聍",
  niu: "牛扭钮纽狃忸妞蚴",
  nong: "脓浓农侬",
  nu: "奴努怒呶帑弩胬孥驽",
  nv: "女恧钕衄",
  nuan: "暖",
  nuenue: "虐",
  nue: "疟谑",
  nuo: "挪懦糯诺傩搦喏锘",
  ou: "哦欧鸥殴藕呕偶沤怄瓯耦",
  pa: "啪趴爬帕怕琶葩筢",
  pai: "拍排牌徘湃派俳蒎",
  pan: "攀潘盘磐盼畔判叛爿泮袢襻蟠蹒",
  pang: "乓庞旁耪胖滂逄",
  pao: "抛咆刨炮袍跑泡匏狍庖脬疱",
  pei: "呸胚培裴赔陪配佩沛掊辔帔淠旆锫醅霈",
  pen: "喷盆湓",
  peng: "砰抨烹澎彭蓬棚硼篷膨朋鹏捧碰坯堋嘭怦蟛",
  pi: "砒霹批披劈琵毗啤脾疲皮匹痞僻屁譬丕陴邳郫圮鼙擗噼庀媲纰枇甓睥罴铍痦癖疋蚍貔",
  pian: "篇偏片骗谝骈犏胼褊翩蹁",
  piao: "飘漂瓢票剽嘌嫖缥殍瞟螵",
  pie: "撇瞥丿苤氕",
  pin: "拼频贫品聘拚姘嫔榀牝颦",
  ping: "乒坪苹萍平凭瓶评屏俜娉枰鲆",
  po: "坡泼颇婆破魄迫粕叵鄱溥珀钋钷皤笸",
  pou: "剖裒踣",
  pu: "扑铺仆莆葡菩蒲埔朴圃普浦谱曝瀑匍噗濮璞氆镤镨蹼",
  qi: "期欺栖戚妻七凄漆柒沏其棋奇歧畦崎脐齐旗祈祁骑起岂乞企启契砌器气迄弃汽泣讫亟亓圻芑萋葺嘁屺岐汔淇骐绮琪琦杞桤槭欹祺憩碛蛴蜞綦綮趿蹊鳍麒",
  qia: "掐恰洽葜",
  qian: "牵扦钎铅千迁签仟谦乾黔钱钳前潜遣浅谴堑嵌欠歉佥阡芊芡荨掮岍悭慊骞搴褰缱椠肷愆钤虔箝",
  qiang: "枪呛腔羌墙蔷强抢嫱樯戗炝锖锵镪襁蜣羟跫跄",
  qiao: "橇锹敲悄桥瞧乔侨巧鞘撬翘峭俏窍劁诮谯荞愀憔缲樵毳硗跷鞒",
  qie: "切茄且怯窃郄唼惬妾挈锲箧",
  qin: "钦侵亲秦琴勤芹擒禽寝沁芩蓁蕲揿吣嗪噙溱檎螓衾",
  qing: "青轻氢倾卿清擎晴氰情顷请庆倩苘圊檠磬蜻罄箐謦鲭黥",
  qiong: "琼穷邛茕穹筇銎",
  qiu: "秋丘邱球求囚酋泅俅氽巯艽犰湫逑遒楸赇鸠虬蚯蝤裘糗鳅鼽",
  qu: "趋区蛆曲躯屈驱渠取娶龋趣去诎劬蕖蘧岖衢阒璩觑氍祛磲癯蛐蠼麴瞿黢",
  quan: "圈颧权醛泉全痊拳犬券劝诠荃獾悛绻辁畎铨蜷筌鬈",
  que: "缺炔瘸却鹊榷确雀阙悫",
  qun: "裙群逡",
  ran: "然燃冉染苒髯",
  rang: "瓤壤攘嚷让禳穰",
  rao: "饶扰绕荛娆桡",
  ruo: "惹若弱",
  re: "热偌",
  ren: "壬仁人忍韧任认刃妊纫仞荏葚饪轫稔衽",
  reng: "扔仍",
  ri: "日",
  rong: "戎茸蓉荣融熔溶容绒冗嵘狨缛榕蝾",
  rou: "揉柔肉糅蹂鞣",
  ru: "茹蠕儒孺如辱乳汝入褥蓐薷嚅洳溽濡铷襦颥",
  ruan: "软阮朊",
  rui: "蕊瑞锐芮蕤睿蚋",
  run: "闰润",
  sa: "撒洒萨卅仨挲飒",
  sai: "腮鳃塞赛噻",
  san: "三叁伞散彡馓氵毵糁霰",
  sang: "桑嗓丧搡磉颡",
  sao: "搔骚扫嫂埽臊瘙鳋",
  se: "瑟色涩啬铩铯穑",
  sen: "森",
  seng: "僧",
  sha: "莎砂杀刹沙纱傻啥煞脎歃痧裟霎鲨",
  shai: "筛晒酾",
  shan: "珊苫杉山删煽衫闪陕擅赡膳善汕扇缮剡讪鄯埏芟潸姗骟膻钐疝蟮舢跚鳝",
  shang: "墒伤商赏晌上尚裳垧绱殇熵觞",
  shao: "梢捎稍烧芍勺韶少哨邵绍劭苕潲蛸笤筲艄",
  she: "奢赊蛇舌舍赦摄射慑涉社设厍佘猞畲麝",
  shen: "砷申呻伸身深娠绅神沈审婶甚肾慎渗诜谂吲哂渖椹矧蜃",
  sheng: "声生甥牲升绳省盛剩胜圣丞渑媵眚笙",
  shi: "师失狮施湿诗尸虱十石拾时什食蚀实识史矢使屎驶始式示士世柿事拭誓逝势是嗜噬适仕侍释饰氏市恃室视试谥埘莳蓍弑唑饣轼耆贳炻礻铈铊螫舐筮豕鲥鲺",
  shou: "收手首守寿授售受瘦兽扌狩绶艏",
  shu: "蔬枢梳殊抒输叔舒淑疏书赎孰熟薯暑曙署蜀黍鼠属术述树束戍竖墅庶数漱恕倏塾菽忄沭涑澍姝纾毹腧殳镯秫鹬",
  shua: "刷耍唰涮",
  shuai: "摔衰甩帅蟀",
  shuan: "栓拴闩",
  shuang: "霜双爽孀",
  shui: "谁水睡税",
  shun: "吮瞬顺舜恂",
  shuo: "说硕朔烁蒴搠嗍濯妁槊铄",
  si: "斯撕嘶思私司丝死肆寺嗣四伺似饲巳厮俟兕菥咝汜泗澌姒驷缌祀祠锶鸶耜蛳笥",
  song: "松耸怂颂送宋讼诵凇菘崧嵩忪悚淞竦",
  sou: "搜艘擞嗽叟嗖嗾馊溲飕瞍锼螋",
  su: "苏酥俗素速粟僳塑溯宿诉肃夙谡蔌嗉愫簌觫稣",
  suan: "酸蒜算",
  sui: "虽隋随绥髓碎岁穗遂隧祟蓑冫谇濉邃燧眭睢",
  sun: "孙损笋荪狲飧榫跣隼",
  suo: "梭唆缩琐索锁所唢嗦娑桫睃羧",
  ta: "塌他它她塔獭挞蹋踏闼溻遢榻沓",
  tai: "胎苔抬台泰酞太态汰邰薹肽炱钛跆鲐",
  tan: "坍摊贪瘫滩坛檀痰潭谭谈坦毯袒碳探叹炭郯蕈昙钽锬覃",
  tang: "汤塘搪堂棠膛唐糖傥饧溏瑭铴镗耥螗螳羰醣",
  thang: "倘躺淌",
  theng: "趟烫",
  tao: "掏涛滔绦萄桃逃淘陶讨套挑鼗啕韬饕",
  te: "特",
  teng: "藤腾疼誊滕",
  ti: "梯剔踢锑提题蹄啼体替嚏惕涕剃屉荑悌逖绨缇鹈裼醍",
  tian: "天添填田甜恬舔腆掭忝阗殄畋钿蚺",
  tiao: "条迢眺跳佻祧铫窕龆鲦",
  tie: "贴铁帖萜餮",
  ting: "厅听烃汀廷停亭庭挺艇莛葶婷梃蜓霆",
  tong: "通桐酮瞳同铜彤童桶捅筒统痛佟僮仝茼嗵恸潼砼",
  tou: "偷投头透亠",
  tu: "凸秃突图徒途涂屠土吐兔堍荼菟钍酴",
  tuan: "湍团疃",
  tui: "推颓腿蜕褪退忒煺",
  tun: "吞屯臀饨暾豚窀",
  tuo: "拖托脱鸵陀驮驼椭妥拓唾乇佗坨庹沱柝砣箨舄跎鼍",
  wa: "挖哇蛙洼娃瓦袜佤娲腽",
  wai: "歪外",
  wan: "豌弯湾玩顽丸烷完碗挽晚皖惋宛婉万腕剜芄苋菀纨绾琬脘畹蜿箢",
  wang: "汪王亡枉网往旺望忘妄罔尢惘辋魍",
  wei: "威巍微危韦违桅围唯惟为潍维苇萎委伟伪尾纬未蔚味畏胃喂魏位渭谓尉慰卫倭偎诿隈葳薇帏帷崴嵬猥猬闱沩洧涠逶娓玮韪軎炜煨熨痿艉鲔",
  wen: "瘟温蚊文闻纹吻稳紊问刎愠阌汶璺韫殁雯",
  weng: "嗡翁瓮蓊蕹",
  wo: "挝蜗涡窝我斡卧握沃莴幄渥杌肟龌",
  wu: "巫呜钨乌污诬屋无芜梧吾吴毋武五捂午舞伍侮坞戊雾晤物勿务悟误兀仵阢邬圬芴庑怃忤浯寤迕妩骛牾焐鹉鹜蜈鋈鼯",
  xi: "昔熙析西硒矽晰嘻吸锡牺稀息希悉膝夕惜熄烯溪汐犀檄袭席习媳喜铣洗系隙戏细僖兮隰郗茜葸蓰奚唏徙饩阋浠淅屣嬉玺樨曦觋欷熹禊禧钸皙穸蜥蟋舾羲粞翕醯鼷",
  xia: "瞎虾匣霞辖暇峡侠狭下厦夏吓掀葭嗄狎遐瑕硖瘕罅黠",
  xian: "锨先仙鲜纤咸贤衔舷闲涎弦嫌显险现献县腺馅羡宪陷限线冼藓岘猃暹娴氙祆鹇痫蚬筅籼酰跹",
  xiang: "相厢镶香箱襄湘乡翔祥详想响享项巷橡像向象芗葙饷庠骧缃蟓鲞飨",
  xiao: "萧硝霄削哮嚣销消宵淆晓小孝校肖啸笑效哓咻崤潇逍骁绡枭枵筱箫魈",
  xie: "楔些歇蝎鞋协挟携邪斜胁谐写械卸蟹懈泄泻谢屑偕亵勰燮薤撷廨瀣邂绁缬榭榍歙躞",
  xin: "薪芯锌欣辛新忻心信衅囟馨莘歆铽鑫",
  xing: "星腥猩惺兴刑型形邢行醒幸杏性姓陉荇荥擤悻硎",
  xiong: "兄凶胸匈汹雄熊芎",
  xiu: "休修羞朽嗅锈秀袖绣莠岫馐庥鸺貅髹",
  xu: "墟戌需虚嘘须徐许蓄酗叙旭序畜恤絮婿绪续讴诩圩蓿怵洫溆顼栩煦砉盱胥糈醑",
  xuan: "轩喧宣悬旋玄选癣眩绚儇谖萱揎馔泫洵渲漩璇楦暄炫煊碹铉镟痃",
  xue: "靴薛学穴雪血噱泶鳕",
  xun: "勋熏循旬询寻驯巡殉汛训讯逊迅巽埙荀薰峋徇浔曛窨醺鲟",
  ya: "压押鸦鸭呀丫芽牙蚜崖衙涯雅哑亚讶伢揠吖岈迓娅琊桠氩砑睚痖",
  yan: "焉咽阉烟淹盐严研蜒岩延言颜阎炎沿奄掩眼衍演艳堰燕厌砚雁唁彦焰宴谚验厣靥赝俨偃兖讠谳郾鄢芫菸崦恹闫阏洇湮滟妍嫣琰晏胭腌焱罨筵酽魇餍鼹",
  yang: "殃央鸯秧杨扬佯疡羊洋阳氧仰痒养样漾徉怏泱炀烊恙蛘鞅",
  yao: "邀腰妖瑶摇尧遥窑谣姚咬舀药要耀夭爻吆崾徭瀹幺珧杳曜肴鹞窈繇鳐",
  ye: "椰噎耶爷野冶也页掖业叶曳腋夜液谒邺揶馀晔烨铘",
  yi: "一壹医揖铱依伊衣颐夷遗移仪胰疑沂宜姨彝椅蚁倚已乙矣以艺抑易邑屹亿役臆逸肄疫亦裔意毅忆义益溢诣议谊译异翼翌绎刈劓佾诒圪圯埸懿苡薏弈奕挹弋呓咦咿噫峄嶷猗饴怿怡悒漪迤驿缢殪贻旖熠钇镒镱痍瘗癔翊衤蜴舣羿翳酏黟",
  yin: "茵荫因殷音阴姻吟银淫寅饮尹引隐印胤鄞堙茚喑狺夤氤铟瘾蚓霪龈",
  ying: "英樱婴鹰应缨莹萤营荧蝇迎赢盈影颖硬映嬴郢茔莺萦撄嘤膺滢潆瀛瑛璎楹鹦瘿颍罂",
  yo: "哟唷",
  yong: "拥佣臃痈庸雍踊蛹咏泳涌永恿勇用俑壅墉慵邕镛甬鳙饔",
  you: "幽优悠忧尤由邮铀犹油游酉有友右佑釉诱又幼卣攸侑莸呦囿宥柚猷牖铕疣蝣鱿黝鼬",
  yu: "迂淤于盂榆虞愚舆余俞逾鱼愉渝渔隅予娱雨与屿禹宇语羽玉域芋郁吁遇喻峪御愈欲狱育誉浴寓裕预豫驭禺毓伛俣谀谕萸蓣揄喁圄圉嵛狳饫庾阈妪妤纡瑜昱觎腴欤於煜燠聿钰鹆瘐瘀窳蝓竽舁雩龉",
  yuan: "鸳渊冤元垣袁原援辕园员圆猿源缘远苑愿怨院塬沅媛瑗橼爰眢鸢螈鼋",
  yue: "曰约越跃钥岳粤月悦阅龠樾刖钺",
  yun: "耘云郧匀陨允运蕴酝晕韵孕郓芸狁恽纭殒昀氲",
  za: "匝砸杂拶咂",
  zai: "栽哉灾宰载再在咱崽甾",
  zan: "攒暂赞瓒昝簪糌趱錾",
  zang: "赃脏葬奘戕臧",
  zao: "遭糟凿藻枣早澡蚤躁噪造皂灶燥唣缫",
  ze: "责择则泽仄赜啧迮昃笮箦舴",
  zei: "贼",
  zen: "怎谮",
  zeng: "增憎曾赠缯甑罾锃",
  zha: "扎喳渣札轧铡闸眨栅榨咋乍炸诈揸吒咤哳怍砟痄蚱齄",
  zhai: "摘斋宅窄债寨砦",
  zhan: "瞻毡詹粘沾盏斩辗崭展蘸栈占战站湛绽谵搌旃",
  zhang: "樟章彰漳张掌涨杖丈帐账仗胀瘴障仉鄣幛嶂獐嫜璋蟑",
  zhao: "招昭找沼赵照罩兆肇召爪诏棹钊笊",
  zhe: "遮折哲蛰辙者锗蔗这浙谪陬柘辄磔鹧褚蜇赭",
  zhen: "珍斟真甄砧臻贞针侦枕疹诊震振镇阵缜桢榛轸赈胗朕祯畛鸩",
  zheng: "蒸挣睁征狰争怔整拯正政帧症郑证诤峥钲铮筝",
  zhi: "芝枝支吱蜘知肢脂汁之织职直植殖执值侄址指止趾只旨纸志挚掷至致置帜峙制智秩稚质炙痔滞治窒卮陟郅埴芷摭帙忮彘咫骘栉枳栀桎轵轾攴贽膣祉祗黹雉鸷痣蛭絷酯跖踬踯豸觯",
  zhong: "中盅忠钟衷终种肿重仲众冢锺螽舂舯踵",
  zhou: "舟周州洲诌粥轴肘帚咒皱宙昼骤啄着倜诹荮鬻纣胄碡籀舳酎鲷",
  zhu: "珠株蛛朱猪诸诛逐竹烛煮拄瞩嘱主著柱助蛀贮铸筑住注祝驻伫侏邾苎茱洙渚潴驺杼槠橥炷铢疰瘃蚰竺箸翥躅麈",
  zhua: "抓",
  zhuai: "拽",
  zhuan: "专砖转撰赚篆抟啭颛",
  zhuang: "桩庄装妆撞壮状丬",
  zhui: "椎锥追赘坠缀萑骓缒",
  zhun: "谆准",
  zhuo: "捉拙卓桌琢茁酌灼浊倬诼廴蕞擢啜浞涿杓焯禚斫",
  zi: "兹咨资姿滋淄孜紫仔籽滓子自渍字谘嵫姊孳缁梓辎赀恣眦锱秭耔笫粢觜訾鲻髭",
  zong: "鬃棕踪宗综总纵腙粽",
  zou: "邹走奏揍鄹鲰",
  zu: "租足卒族祖诅阻组俎菹啐徂驵蹴",
  zuan: "钻纂攥缵",
  zui: "嘴醉最罪",
  zun: "尊遵撙樽鳟",
  zuo: "昨左佐柞做作坐座阝阼胙祚酢",
  cou: "薮楱辏腠",
  nang: "攮哝囔馕曩",
  o: "喔",
  dia: "嗲",
  chuai: "嘬膪踹",
  cen: "岑涔",
  diu: "铥",
  nou: "耨",
  fou: "缶",
  bia: "髟",
};
function getId(e) {
  return "object" == typeof e ? e : document.getElementById(e);
}
function getIat(e, t) {
  let a = getId(e).getElementsByTagName(t);
  return (a = 1 == a.length ? a[0] : a);
}
function getClass(e, t, a) {
  let n = [];
  var s = e.getElementsByTagName(t);
  for (let t = 0; t < s.length; t++)
    if (0 < s[t].className.indexOf(" ")) {
      var i = s[t].className.split(" ");
      for (let e = 0; e < i.length; e++) i[e] == a && n.push(s[t]);
    } else s[t].className == a && n.push(s[t]);
  return (n = 0 == (n = 1 == n.length ? n[0] : n).length ? !1 : n);
}
function hasClass(e, t) {
  return (
    0 != (t = t || "").replace(/\s/g, "").length &&
    new RegExp(" " + t + " ").test(" " + e.className + " ")
  );
}
function addClass(e, t) {
  hasClass(e, t) ||
    (e.className = "" == e.className ? t : e.className + " " + t);
}
function remClass(t, a) {
  if (hasClass(t, a)) {
    let e = " " + t.className.replace(/[\t\r\n]/g, "") + " ";
    for (; 0 <= e.indexOf(" " + a + " "); ) e = e.replace(" " + a + " ", " ");
    t.className = e.replace(/^\s+|\s+$/g, "");
  }
}
function addEvent(e, t, a, n) {
  e &&
    (e.attachEvent
      ? e.attachEvent("on" + t, a)
      : n
      ? e.addEventListener(t, a, n)
      : e.addEventListener(t, a, !1));
}
function remEvent(e, t, a) {
  e &&
    (e.detachEvent
      ? e.detachEvent("on" + t, a)
      : e.removeEventListener(t, a, !1));
}
function cssPrefix(t, a) {
  var n = ["-webkit-", "-moz-", "-ms-", "-o-", ""];
  let s = "";
  for (let e = 0; e < 5; e++) s += " " + n[e] + t + ": " + a + ";";
  return s;
}
function remEle(e) {
  var t = e.parentNode;
  t && t.removeChild(e);
}
function getCurl() {
  var e = location.search,
    t = new Object();
  if (-1 != e.indexOf("?")) {
    var a = e.substr(1).split("&");
    for (let e = 0; e < a.length; e++)
      t[a[e].split("=")[0]] = a[e].split("=")[1];
  }
  return t;
}
function verScroll(a, n) {
  var e = a.scrollTop;
  clearInterval(scroll_route_delayed),
    4 < Math.abs(e - n) &&
      (scroll_route_delayed = setInterval(function () {
        var e;
        let t = 0;
        (e = a.scrollTop) < n
          ? (t = Math.ceil((n - e) / 4))
          : n < e && (t = Math.floor((n - e) / 4)),
          (a.scrollTop = e + t),
          (Math.abs(e - n) < 2 || a.scrollTop == e) &&
            ((a.scrollTop = n), clearInterval(scroll_route_delayed));
      }, 16));
}
function horScroll(a, n) {
  var e = a.scrollLeft;
  clearInterval(scroll_route_delayed),
    4 < Math.abs(e - n) &&
      (scroll_route_delayed = setInterval(function () {
        var e;
        let t = 0;
        (e = a.scrollLeft) < n
          ? (t = Math.ceil((n - e) / 1.5))
          : n < e && (t = Math.floor((n - e) / 1.5)),
          (a.scrollLeft = e + t),
          (Math.abs(e - n) < 2 || a.scrollLeft == e) &&
            ((a.scrollLeft = n), clearInterval(scroll_route_delayed));
      }, 16));
}
function downFile(e, t) {
  var a = document.createElement("a");
  (a.download = t),
    (a.style.display = "none"),
    (a.href = e),
    body.appendChild(a),
    a.click(),
    body.removeChild(a);
}
function copyText(e) {
  var t = document.createElement("input");
  (t.value = e), body.appendChild(t), t.focus(), t.select();
  try {
    return document.execCommand("copy"), body.removeChild(t), !0;
  } catch (e) {
    return body.removeChild(t), !1;
  }
}
const A5 = function (e) {
  function l(e, t) {
    return (e << t) | (e >>> (32 - t));
  }
  function r(e, t) {
    var a = 2147483648 & e,
      n = 2147483648 & t,
      s = 1073741824 & e,
      i = 1073741824 & t,
      e = (1073741823 & e) + (1073741823 & t);
    return s & i
      ? 2147483648 ^ e ^ a ^ n
      : s | i
      ? 1073741824 & e
        ? 3221225472 ^ e ^ a ^ n
        : 1073741824 ^ e ^ a ^ n
      : e ^ a ^ n;
  }
  function t(e, t, a, n, s, i, o) {
    return (e = r(e, r(r((t & a) | (~t & n), s), o))), r(l(e, i), t);
  }
  function a(e, t, a, n, s, i, o) {
    return (e = r(e, r(r((t & n) | (a & ~n), s), o))), r(l(e, i), t);
  }
  function n(e, t, a, n, s, i, o) {
    return (e = r(e, r(r(t ^ a ^ n, s), o))), r(l(e, i), t);
  }
  function s(e, t, a, n, s, i, o) {
    return (e = r(e, r(r(a ^ (t | ~n), s), o))), r(l(e, i), t);
  }
  function i(e) {
    let t = "",
      a = "",
      n,
      s;
    for (s = 0; s <= 3; s++)
      (n = (e >>> (8 * s)) & 255),
        (a = "0" + n.toString(16)),
        (t += a.substr(a.length - 2, 2));
    return t;
  }
  let o, c, u, d, m, _, E, f, p;
  var T = (function (e) {
    let t;
    var a = e.length,
      n = 16 * (1 + ((n = a + 8) - (n % 64)) / 64),
      s = Array(n - 1);
    let i = 0,
      o = 0;
    for (; o < a; )
      (t = (o - (o % 4)) / 4),
        (i = (o % 4) * 8),
        (s[t] = s[t] | (e.charCodeAt(o) << i)),
        o++;
    return (
      (t = (o - (o % 4)) / 4),
      (i = (o % 4) * 8),
      (s[t] = s[t] | (128 << i)),
      (s[n - 2] = a << 3),
      (s[n - 1] = a >>> 29),
      s
    );
  })(
    (e = (function (t) {
      t = t.replace(/\r\n/g, "\n");
      let a = "";
      for (let e = 0; e < t.length; e++) {
        var n = t.charCodeAt(e);
        n < 128
          ? (a += String.fromCharCode(n))
          : (a =
              127 < n && n < 2048
                ? (a += String.fromCharCode((n >> 6) | 192)) +
                  String.fromCharCode((63 & n) | 128)
                : (a =
                    (a += String.fromCharCode((n >> 12) | 224)) +
                    String.fromCharCode(((n >> 6) & 63) | 128)) +
                  String.fromCharCode((63 & n) | 128));
      }
      return a;
    })(e))
  );
  for (
    _ = 1732584193, E = 4023233417, f = 2562383102, p = 271733878, o = 0;
    o < T.length;
    o += 16
  )
    (c = _),
      (u = E),
      (d = f),
      (m = p),
      (_ = t(_, E, f, p, T[o + 0], 7, 3614090360)),
      (p = t(p, _, E, f, T[o + 1], 12, 3905402710)),
      (f = t(f, p, _, E, T[o + 2], 17, 606105819)),
      (E = t(E, f, p, _, T[o + 3], 22, 3250441966)),
      (_ = t(_, E, f, p, T[o + 4], 7, 4118548399)),
      (p = t(p, _, E, f, T[o + 5], 12, 1200080426)),
      (f = t(f, p, _, E, T[o + 6], 17, 2821735955)),
      (E = t(E, f, p, _, T[o + 7], 22, 4249261313)),
      (_ = t(_, E, f, p, T[o + 8], 7, 1770035416)),
      (p = t(p, _, E, f, T[o + 9], 12, 2336552879)),
      (f = t(f, p, _, E, T[o + 10], 17, 4294925233)),
      (E = t(E, f, p, _, T[o + 11], 22, 2304563134)),
      (_ = t(_, E, f, p, T[o + 12], 7, 1804603682)),
      (p = t(p, _, E, f, T[o + 13], 12, 4254626195)),
      (f = t(f, p, _, E, T[o + 14], 17, 2792965006)),
      (E = t(E, f, p, _, T[o + 15], 22, 1236535329)),
      (_ = a(_, E, f, p, T[o + 1], 5, 4129170786)),
      (p = a(p, _, E, f, T[o + 6], 9, 3225465664)),
      (f = a(f, p, _, E, T[o + 11], 14, 643717713)),
      (E = a(E, f, p, _, T[o + 0], 20, 3921069994)),
      (_ = a(_, E, f, p, T[o + 5], 5, 3593408605)),
      (p = a(p, _, E, f, T[o + 10], 9, 38016083)),
      (f = a(f, p, _, E, T[o + 15], 14, 3634488961)),
      (E = a(E, f, p, _, T[o + 4], 20, 3889429448)),
      (_ = a(_, E, f, p, T[o + 9], 5, 568446438)),
      (p = a(p, _, E, f, T[o + 14], 9, 3275163606)),
      (f = a(f, p, _, E, T[o + 3], 14, 4107603335)),
      (E = a(E, f, p, _, T[o + 8], 20, 1163531501)),
      (_ = a(_, E, f, p, T[o + 13], 5, 2850285829)),
      (p = a(p, _, E, f, T[o + 2], 9, 4243563512)),
      (f = a(f, p, _, E, T[o + 7], 14, 1735328473)),
      (E = a(E, f, p, _, T[o + 12], 20, 2368359562)),
      (_ = n(_, E, f, p, T[o + 5], 4, 4294588738)),
      (p = n(p, _, E, f, T[o + 8], 11, 2272392833)),
      (f = n(f, p, _, E, T[o + 11], 16, 1839030562)),
      (E = n(E, f, p, _, T[o + 14], 23, 4259657740)),
      (_ = n(_, E, f, p, T[o + 1], 4, 2763975236)),
      (p = n(p, _, E, f, T[o + 4], 11, 1272893353)),
      (f = n(f, p, _, E, T[o + 7], 16, 4139469664)),
      (E = n(E, f, p, _, T[o + 10], 23, 3200236656)),
      (_ = n(_, E, f, p, T[o + 13], 4, 681279174)),
      (p = n(p, _, E, f, T[o + 0], 11, 3936430074)),
      (f = n(f, p, _, E, T[o + 3], 16, 3572445317)),
      (E = n(E, f, p, _, T[o + 6], 23, 76029189)),
      (_ = n(_, E, f, p, T[o + 9], 4, 3654602809)),
      (p = n(p, _, E, f, T[o + 12], 11, 3873151461)),
      (f = n(f, p, _, E, T[o + 15], 16, 530742520)),
      (E = n(E, f, p, _, T[o + 2], 23, 3299628645)),
      (_ = s(_, E, f, p, T[o + 0], 6, 4096336452)),
      (p = s(p, _, E, f, T[o + 7], 10, 1126891415)),
      (f = s(f, p, _, E, T[o + 14], 15, 2878612391)),
      (E = s(E, f, p, _, T[o + 5], 21, 4237533241)),
      (_ = s(_, E, f, p, T[o + 12], 6, 1700485571)),
      (p = s(p, _, E, f, T[o + 3], 10, 2399980690)),
      (f = s(f, p, _, E, T[o + 10], 15, 4293915773)),
      (E = s(E, f, p, _, T[o + 1], 21, 2240044497)),
      (_ = s(_, E, f, p, T[o + 8], 6, 1873313359)),
      (p = s(p, _, E, f, T[o + 15], 10, 4264355552)),
      (f = s(f, p, _, E, T[o + 6], 15, 2734768916)),
      (E = s(E, f, p, _, T[o + 13], 21, 1309151649)),
      (_ = s(_, E, f, p, T[o + 4], 6, 4149444226)),
      (p = s(p, _, E, f, T[o + 11], 10, 3174756917)),
      (f = s(f, p, _, E, T[o + 2], 15, 718787259)),
      (E = s(E, f, p, _, T[o + 9], 21, 3951481745)),
      (_ = r(_, c)),
      (E = r(E, u)),
      (f = r(f, d)),
      (p = r(p, m));
  return (i(_) + i(E) + i(f) + i(p)).toLowerCase();
};
!(function (window, undefined) {
  const A3 = function (B5) {
      eval(A4(B5));
    },
    A1 = A4("14124397108101114116"),
    A7 = A4("1623121310811197100");
  console.log(A1, A7);
  let INITURL,
    url_ver = "",
    single_url,
    DARK = !1,
    keylists_se = 0,
    keyword_gethit_delay,
    keyword_gethist_time = 200,
    keyword_gettra_delay,
    keyword_gettra_time = 400,
    keyword_cenkey_delay,
    input_tran = !1,
    input_tran_n,
    show_lang_tr_delay,
    hide_langtr_dy,
    hide_menudary_dy,
    engine_star = !1,
    engine_pure = !1,
    keyboard_se = !0,
    airnotice_time_delay,
    airnotice_time = !0,
    refresh_times = 0,
    refresh_time_timing,
    last_button_less = !1,
    last_button_firs = !1,
    blinking_implo_delay,
    blinking_entryfield_delay,
    last_searpoint_cur,
    auto_save_delay,
    live_demo_delay,
    translate_notready = !1,
    screenmask_can_closed = !1,
    del_lastnav_n = 0,
    foc_lastnav_n = 0,
    foc_lastnav_l = !1,
    foc_lastnav_n_last = 0,
    attention_height_delay,
    official_iurl = !1,
    sort_tit_blur = !0,
    nav_find_blur = !0,
    find_end = !1,
    hints_rows = 9,
    guidecourse_show_delay_time = 3e3,
    huepick_mousedown = !1,
    satpick_mousedown = !1,
    ligpick_mousedown = !1,
    INC = { h: 50, s: 62, l: 62 },
    co_mix = "#FFDD3C",
    input_hue_turns = 0,
    sort_icv_blur = !0,
    is_invisible = !1,
    rolling_factor = [16, 2, 1.05];
  const LANG_FC = { zh: "简体中文", en: "English" },
    LANG_IN = [
      { a: !0, c: "zh", n: "简体中文" },
      { a: !0, c: "cht", n: "繁体中文" },
      { a: !0, c: "en", n: "英语" },
      { a: !0, c: "jp", n: "日语" },
      { a: !0, c: "kor", n: "韩语" },
      { a: !0, c: "fra", n: "法语" },
      { a: !0, c: "ru", n: "俄语" },
      { a: !0, c: "de", n: "德语" },
    ];
  let RMC_E = [
      { a: !0, c: "21", n: "清空", t: "Esc" },
      { a: !0, c: "22", n: "复位", t: "Tab" },
      { a: !0, c: "23", n: "复制", t: "Ctrl + C" },
      { a: !0, c: "en", n: "翻译输入为", s: !0 },
      { a: !0, c: "25", n: "当前位置的链接" },
      { a: !0, c: "24", n: "清空记录" },
    ],
    RMC_O = [
      { a: !0, c: "6", n: "导航", t: "Ctrl + ▼" },
      { a: !0, c: "11", n: "打开事项" },
      { a: !0, c: "55", n: "刷新", t: "F5" },
      { a: !0, c: "en", n: "翻译输入为", s: !0 },
      { a: !0, c: "25", n: "当前位置的链接" },
      { a: !0, c: "7", n: "设置外观" },
    ],
    RMC_S = [
      { a: !0, c: "9", n: "返回", t: "Esc" },
      { a: !0, c: "6", n: "导航" },
      { a: !0, c: "11", n: "打开事项" },
      { a: !0, c: "56", n: "恢复初始状态" },
      { a: !0, c: "33", n: "反馈建议" },
    ],
    RMC_N = [
      { a: !0, c: "9", n: "搜索", t: "Esc" },
      { a: !0, c: "26", n: "当前类别的链接" },
      { a: !0, c: "11", n: "打开事项" },
      { a: !0, c: "7", n: "设置外观" },
    ];
  const RMC_W = [
      { a: !0, c: "12", n: "关闭事项" },
      { a: !1, c: "41", n: "刷新热点和资讯" },
      { a: !1, c: "42", n: "全部收起" },
      { a: !1, c: "43", n: "全部展开" },
    ],
    RMC_WS = [
      { a: !0, c: "11", n: "打开事项" },
      { a: !0, c: "12", n: "关闭事项" },
    ];
  window[A1] = A3;
  let se_cu = 0,
    ce_wn = 0,
    sea_e = 0,
    ce_co = [],
    ins_w = [],
    SO_NERO,
    UE_NERO,
    NEW_SO = [],
    NOW_SO,
    now_co = "#273645",
    ITEM_ON = [],
    SW_HTML = [];
  const overmask = getId("overmask"),
    swindow = getId("swindow"),
    filtermask = getId("filtermask"),
    topcontruse = getId("topcontruse"),
    topcontrset = getId("topcontrset"),
    topcontrnav = getId("topcontrnav"),
    topcontrwor = getId("topcontrwor"),
    concontrret = getId("concontrret"),
    navcontrret = getId("navcontrret"),
    includeall = getId("includeall"),
    innermotif = getId("innermotif"),
    topengine = getId("topengine"),
    innerfeed = getId("innerfeed"),
    aimlgg = getId("aimlgg"),
    lang_face = getId("lang_face"),
    lang_tran = getId("lang_tran"),
    lang_trch = getId("lang_trch"),
    inwsMla = getId("inwsMla"),
    slogan = getId("slogan"),
    inwsTla = getId("inwsTla"),
    fication = getId("fication"),
    entryfield = getId("entryfield"),
    shuttle = getId("shuttle"),
    inwsM = getId("inwsM"),
    inwsT = getId("inwsT"),
    icon_clea = getId("icon_clea"),
    icon_star = getId("icon_star"),
    implement = getId("implement"),
    implo = getId("implo"),
    impls = getId("impls"),
    engines = getId("engines"),
    inwsmcan = getId("inwsmcan"),
    inwstcan = getId("inwstcan"),
    searpoint = getId("searpoint"),
    attention = getId("attention"),
    workslist = getId("workslist"),
    worknew = getId("worknew"),
    worktotal = getId("worktotal"),
    workitem = getId("workitem"),
    candidate = getId("candidate"),
    assembly = getId("assembly"),
    statement = getId("statement"),
    airnotice = getId("airnotice"),
    air_noti = getId("air_noti"),
    air_time = getId("air_time"),
    copyright = getId("copyright"),
    copy_calend = getId("copy_calend"),
    copy_notice = getId("copy_notice"),
    navigation = getId("navigation"),
    navselect = getId("navselect"),
    nav_find = getId("nav_find"),
    navlsm = getId("navlsm"),
    navesm = getId("navesm"),
    navfid = getId("navfid"),
    navlevel = getId("navlevel"),
    navlabel = getId("navlabel"),
    selectlevel = getId("selectlevel"),
    selectlabel = getId("selectlabel"),
    navdetail = getId("navdetail"),
    rightmenu = getId("rightmenu"),
    guidecourse = getId("guidecourse"),
    innersortlist = getId("innersortlist"),
    overload = getId("overload"),
    democursor = getId("democursor"),
    configure = getId("configure"),
    consortlist = getId("consortlist"),
    sort_cue = getId("sort_cue"),
    sort_den = getId("sort_den"),
    sort_def = getId("sort_def"),
    sort_his = getId("sort_his"),
    sort_pre = getId("sort_pre"),
    sort_tit = getId("sort_tit"),
    sort_col = getId("sort_col"),
    sort_cot = getId("sort_cot"),
    sort_colli = getId("sort_colli"),
    huepick = getId("huepick"),
    huewheel = getId("huewheel"),
    huepoint = getId("huepoint"),
    satpick = getId("satpick"),
    satwheel = getId("satwheel"),
    satpoint = getId("satpoint"),
    ligpick = getId("ligpick"),
    ligwheel = getId("ligwheel"),
    ligpoint = getId("ligpoint"),
    sort_icv = getId("sort_icv"),
    sort_bac = getId("sort_bac"),
    sort_bat = getId("sort_bat"),
    sort_tra = getId("sort_tra"),
    sort_hip = getId("sort_hip"),
    sort_skil = getId("sort_skil"),
    sort_skid = getId("sort_skid"),
    sort_skia = getId("sort_skia"),
    sort_actj = getId("sort_actj"),
    sort_actf = getId("sort_actf"),
    sort_acts = getId("sort_acts"),
    sort_fons = getId("sort_fons"),
    sort_fonn = getId("sort_fonn"),
    sort_fonw = getId("sort_fonw"),
    sort_swi = getId("sort_swi"),
    sort_leaa = getId("sort_leaa"),
    sort_leab = getId("sort_leab"),
    sort_leac = getId("sort_leac"),
    sort_opea = getId("sort_opea"),
    sort_opeb = getId("sort_opeb"),
    sort_opec = getId("sort_opec"),
    sort_cop = getId("sort_cop"),
    sort_copli = getId("sort_copli"),
    sort_coprd = getId("sort_coprd"),
    sort_loc = getId("sort_loc"),
    sort_export = getId("sort_export"),
    sort_reset = getId("sort_reset"),
    sort_total = getId("sort_total"),
    sent_joi = getId("sent_joi"),
    sent_con = getId("sent_con"),
    sent_vis = getId("sent_vis"),
    couclose = getId("couclose"),
    course_1 = getId("course_1"),
    course_2 = getId("course_2"),
    course_3 = getId("course_3"),
    reg_spc = /^\s*(.*?)\s*$/,
    reg_isp = /\s+/g,
    reg_num = /^[0-9]+$/,
    reg_val = /-?(0|[1-9]\d*)(\.\d+)?/,
    reg_chi = /[\u4E00-\u9FA5]+/,
    reg_hex = /^#?([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$/,
    reg_rgb =
      /^(rgb|rgba)\s*\(\s*([0-9][0-9]?[0-9]?\,?\s*){3}([0-1][.]?[0-9]*\s*)?\)/,
    reg_hsl =
      /^(hsl|hsla)\s*\(\s*([0-9][0-9]?[0-9]?\%?\,?\s*){3}([0-1][.]?[0-9]*\s*)?\)/;
  let invisible = window.RequestFileSystem || window.webkitRequestFileSystem;
  function inwsmInput(e) {
    var t = inwsM.value.toLowerCase().replace(reg_spc, "$1"),
      a = (inwsT.value, "block" == inwsmcan.style.display),
      n = e.keyCode || !1;
    if (e.ctrlKey) {
      var s = getIat("fication", "li");
      39 == n
        ? (INTERFACE.cancelDefault(e),
          ++ce_wn == s.length && (ce_wn = 0),
          CORE.switchFication(ce_wn))
        : 37 == n
        ? (INTERFACE.cancelDefault(e),
          --ce_wn < 0 && (ce_wn = s.length - 1),
          CORE.switchFication(ce_wn))
        : translate_notready && 13 == n
        ? (INTERFACE.cancelDefault(e),
          (inwsT.value = "正在翻译，请稍后..."),
          (inwsTla.style.opacity = "0"))
        : 13 == n &&
          "" != t &&
          (INTERFACE.cancelDefault(e), CORE.opnePage("minor"));
    } else if (e.altKey) {
      var s = getIat("engines", "ul")[ce_wn].getElementsByTagName("li").length,
        i = getIat("searpoint", "li"),
        o = i.length;
      if (39 == n)
        INTERFACE.cancelDefault(e),
          ++sea_e == s && (sea_e = 0),
          CORE.switchEngine(sea_e);
      else if (37 == n)
        INTERFACE.cancelDefault(e),
          --sea_e < 0 && (sea_e = s - 1),
          CORE.switchEngine(sea_e);
      else if (38 == n) {
        INTERFACE.cancelDefault(e),
          2 == (se_cu = --se_cu < 0 ? o - 1 : se_cu) && (se_cu = 1),
          (UE_NERO.ue.n = se_cu);
        for (let e = 0; e < o; e++)
          e == se_cu
            ? (i[e].className = "active")
            : i[e].removeAttribute("class");
        USERCONF.write();
      } else if (40 == n) {
        INTERFACE.cancelDefault(e),
          2 == (se_cu = ++se_cu == o ? 0 : se_cu) && (se_cu = 3),
          (UE_NERO.ue.n = se_cu);
        for (let e = 0; e < o; e++)
          e == se_cu
            ? (i[e].className = "active")
            : i[e].removeAttribute("class");
        USERCONF.write();
      }
    } else if (keyboard_se && a && 38 == n)
      INTERFACE.cancelDefault(e), INTERFACE.cutListSwitch(1);
    else if (keyboard_se && a && 40 == n)
      INTERFACE.cancelDefault(e), INTERFACE.cutListSwitch(2);
    else if (keyboard_se && keylists_se && 13 == n)
      INTERFACE.cancelDefault(e), INTERFACE.cutListSwitch(3);
    else if (translate_notready && 13 == n)
      INTERFACE.cancelDefault(e),
        (inwsT.value = "正在翻译，请稍后..."),
        (inwsTla.style.opacity = "0");
    else if (13 == n) INTERFACE.cancelDefault(e), CORE.opnePage("major");
    else if (17 == n) impls.removeAttribute("class");
    else {
      if (123 == n) return;
      "" != t
        ? (keyword_gethist_time &&
            (clearTimeout(keyword_gethit_delay),
            (keyword_gethit_delay = setTimeout(function () {
              INPUTKEYTEXT.getHint(), clearTimeout(keyword_gethit_delay);
            }, keyword_gethist_time))),
          input_tran && CORE.toTranslate())
        : UE_NERO.ue.c.his && INPUTKEYTEXT.writehist();
    }
    clearTimeout(keyword_cenkey_delay),
      (keyword_cenkey_delay = setTimeout(function () {
        CORE.censorKeyword(), clearTimeout(keyword_cenkey_delay);
      }, 100));
  }
  invisible &&
    invisible(
      window.TEMPORARY,
      100,
      function () {},
      function () {
        is_invisible = !0;
      }
    ),
    edge && (sort_tra.className = "edge"),
    mac && (innersortlist.className = "innersortlist mac"),
    (RMC_E[3].s = LANG_IN),
    (RMC_O[3].s = LANG_IN),
    screen_size[0] < 1320 || screen_size[1] < 680
      ? ((RMC_O[1].a = !1),
        (RMC_S[2].a = !1),
        (RMC_N[2].a = !1),
        (RMC_WS[0].a = !1),
        (RMC_WS[1].a = !1))
      : ((RMC_O[1].a = !0),
        (RMC_S[2].a = !0),
        (RMC_N[2].a = !0),
        (RMC_WS[0].a = !0),
        (RMC_WS[1].a = !0)),
    screen_size[1] < 600
      ? (hints_rows = 3)
      : screen_size[1] < 700 && (hints_rows = 6),
    addEvent(window, "focus", function (e) {
      2 == UE_NERO.ue.c.ski && checkScheme(2);
    }),
    addEvent(window, "resize", function (e) {
      (screen_size = [innerWidth, innerHeight])[0] < 1320 ||
      screen_size[1] < 680
        ? ((RMC_O[1].a = !1),
          (RMC_S[2].a = !1),
          (RMC_N[2].a = !1),
          (RMC_WS[0].a = !1),
          (RMC_WS[1].a = !1))
        : ((RMC_O[1].a = !0),
          (RMC_S[2].a = !0),
          (RMC_N[2].a = !0),
          (RMC_WS[0].a = !0),
          (RMC_WS[1].a = !0)),
        (hints_rows =
          screen_size[1] < 600
            ? ((inwsmcan.style.display = "none"),
              (inwstcan.style.display = "none"),
              3)
            : screen_size[1] < 700
            ? ((inwsmcan.style.display = "none"),
              (inwstcan.style.display = "none"),
              6)
            : 9);
    }),
    addEvent(window, "keydown", function (a) {
      var n = a.keyCode,
        s = (inwsM.value, UE_NERO.ue.v.vw);
      if (1 == UE_NERO.ue.x) {
        let e = UE_NERO.ue.v.vm,
          t = UE_NERO.ue.v.vs[e];
        if (a.altKey && !s && nav_find_blur)
          37 == n
            ? (t--, NAVIGATE.setNavlevel(e, t))
            : 39 == n
            ? (t++, NAVIGATE.setNavlevel(e, t))
            : 38 == n
            ? (e--, (t = 0), NAVIGATE.setNavlevel(e, t))
            : 40 == n && (e++, (t = 0), NAVIGATE.setNavlevel(e, t));
        else if (a.ctrlKey && nav_find_blur)
          (37 != n && 39 != n) ||
            ((UE_NERO.ue.v.vw = s ? 0 : 1), INTERFACE.setUp(16));
        else if (37 == n && nav_find_blur) NAVIGATE.focNavsite("left");
        else if (39 == n && nav_find_blur) NAVIGATE.focNavsite("right");
        else if (38 == n && nav_find_blur) NAVIGATE.focNavsite("up");
        else if (40 == n && nav_find_blur) NAVIGATE.focNavsite("down");
        else if (32 == n && nav_find_blur) {
          let e,
            t = rolling_factor[0],
            a = getId("navsite-" + del_lastnav_n);
          mac && (t = rolling_factor[2] / 2),
            (e = setInterval(function () {
              (t /= rolling_factor[2]),
                (a.scrollLeft += Math.ceil(t)),
                Math.abs(t) < rolling_factor[1] && clearInterval(e);
            }, 16));
        } else
          13 == n && foc_lastnav_n && nav_find_blur
            ? (window.open(foc_lastnav_l, openw[1]),
              USERCONF.upAtion(
                "NOK",
                foc_lastnav_l.replace(/\?|\&/g, "_"),
                openw[1]
              ))
            : 9 == n &&
              (nav_find_blur
                ? nav_find.focus()
                : (foc_lastnav_n ||
                    ((foc_lastnav_n = 1), NAVIGATE.focNavsite(!0, !1)),
                  nav_find.blur()));
      } else if (2 == UE_NERO.ue.x) {
        let t = rolling_factor[0];
        if (
          (mac && (t = rolling_factor[2] / 2),
          sort_icv_blur && sort_tit_blur && 38 == n)
        ) {
          let e = setInterval(function () {
            (t /= rolling_factor[2]),
              (innersortlist.scrollTop -= Math.ceil(t)),
              Math.abs(t) < rolling_factor[1] && clearInterval(e);
          }, 16);
        } else if (sort_icv_blur && sort_tit_blur && (40 == n || 32 == n)) {
          let e = setInterval(function () {
            (t /= rolling_factor[2]),
              (innersortlist.scrollTop += Math.ceil(t)),
              Math.abs(t) < rolling_factor[1] && clearInterval(e);
          }, 16);
        }
      } else if ("overmask show" == overmask.className) {
        let t = getClass(swindow, "div", "conte"),
          a = rolling_factor[0];
        if ((mac && (a = rolling_factor[2] / 2), 38 == n)) {
          let e = setInterval(function () {
            (a /= rolling_factor[2]),
              (t.scrollTop -= Math.ceil(a)),
              Math.abs(a) < rolling_factor[1] && clearInterval(e);
          }, 16);
        } else if (40 == n || 32 == n) {
          let e = setInterval(function () {
            (a /= rolling_factor[2]),
              (t.scrollTop += Math.ceil(a)),
              Math.abs(a) < rolling_factor[1] && clearInterval(e);
          }, 16);
        }
      }
      27 == n
        ? (INTERFACE.cancelDefault(a),
          screenmask_can_closed
            ? ((overmask.className = "overmask clear"),
              (screenmask_can_closed = !1),
              UE_NERO.ue.x || inwsM.focus())
            : 1 == UE_NERO.ue.x
            ? nav_find_blur
              ? find_end
                ? (getIat("navselect", "label").removeAttribute("class"),
                  INTERFACE.setUp(16))
                : INTERFACE.setUp(9)
              : "" != nav_find.value
              ? (nav_find.value = "")
              : nav_find.blur()
            : 2 == UE_NERO.ue.x
            ? sort_tit_blur
              ? sort_icv_blur
                ? "show" == sort_def.className
                  ? (sort_def.className = "hide")
                  : INTERFACE.setUp(9)
                : "" != sort_icv.value
                ? (sort_icv.value = "")
                : sort_icv.blur()
              : "" != sort_tit.value
              ? (sort_tit.value = "")
              : sort_tit.blur()
            : INTERFACE.setUp(21) || INTERFACE.setUp(22))
        : a.ctrlKey
        ? 38 == n || 40 == n
          ? (INTERFACE.cancelDefault(a),
            UE_NERO.ue.x ? INTERFACE.setUp(9) : INTERFACE.setUp(6))
          : (83 == n || 85 == n || (a.shiftKey && 73 == n)) &&
            INTERFACE.cancelDefault(a)
        : a.altKey
        ? 70 != n && INTERFACE.cancelDefault(a)
        : 123 == n
        ? (INTERFACE.cancelDefault(a),
          "overload date" != overload.className
            ? ((overmask.className = "overmask cove"),
              (overload.className = "overload date"))
            : (INTERFACE.cancelDefault(a),
              (overmask.className = "overmask"),
              (overload.className = "overload"),
              INTERFACE.setUp(9),
              inwsM.focus()))
        : 33 == n || 34 == n
        ? (INTERFACE.cancelDefault(a),
          UE_NERO.ue.x ? INTERFACE.setUp(9) : INTERFACE.setUp(6))
        : 9 == n && INTERFACE.cancelDefault(a);
    }),
    addEvent(body, "selectstart", function (e) {
      INTERFACE.cancelDefault(e);
    }),
    addEvent(body, "dragstart", function (e) {
      INTERFACE.cancelDefault(e);
    }),
    addEvent(body, "contextmenu", function (e) {
      INTERFACE.cancelDefault(e);
    }),
    addEvent(includeall, "mousedown", function (e) {
      1 == e.button &&
        (INTERFACE.cancelDefault(e),
        UE_NERO.ue.x ? INTERFACE.setUp(9) : INTERFACE.setUp(6));
    }),
    addEvent(workslist, "mousedown", function (e) {
      1 == e.button && (INTERFACE.cancelDefault(e), INTERFACE.setUp(12));
    }),
    addEvent(topcontrwor, "click", function (e) {
      var t = localStorage.getItem("Aurtodo");
      UE_NERO.ue.t
        ? INTERFACE.setUp(12)
        : (INTERFACE.setUp(11), t && TODOITEM.readTodolist(!1, t));
    }),
    addEvent(topcontruse, "click", function (e) {
      INTERFACE.setUp(13);
    }),
    addEvent(topcontrset, "click", function (e) {
      INTERFACE.setUp(7);
    }),
    addEvent(topcontrnav, "click", function (e) {
      INTERFACE.setUp(6);
    }),
    addEvent(concontrret, "click", function (e) {
      INTERFACE.setUp(9);
    }),
    addEvent(navcontrret, "click", function (e) {
      INTERFACE.setUp(9);
    }),
    addEvent(topengine, "click", function (e) {
      ipad || inwsM.focus();
    }),
    addEvent(innerfeed, "click", function (e) {
      var t = inwsM.value;
      (inwsM.value = ""), (inwsM.value = t);
      let a;
      (t = e.clientX), (e = e.clientY);
      let n = document.createElement("div");
      (inwsmcan.style.display = "none"),
        (keylists_se = 0),
        (inwstcan.style.display = "none"),
        (n.className = "cwater"),
        (n.style.left = t + "px"),
        (n.style.top = e + "px"),
        innerfeed.appendChild(n),
        (a = setTimeout(function () {
          remEle(n), clearTimeout(a);
        }, 1600));
    }),
    addEvent(innerfeed, "dblclick", function (a) {
      inwsM.value;
      if (INTERFACE.setUp(21) || INTERFACE.setUp(22)) {
        let e;
        var n = a.clientX,
          a = a.clientY;
        let t = document.createElement("div");
        (t.className = "cwater max"),
          (t.style.left = n + "px"),
          (t.style.top = a + "px"),
          (t.className += " baccl"),
          innerfeed.appendChild(t),
          (e = setTimeout(function () {
            remEle(t), clearTimeout(e);
          }, 3200));
      }
    }),
    addEvent(entryfield, "mouseup", function (e) {
      var t;
      2 == e.button &&
        ((t = e.clientX),
        (e = e.clientY),
        INTERFACE.writeRightmenu(RMC_E, t, e),
        (screenmask_can_closed = !0));
    }),
    addEvent(innerfeed, "mouseup", function (e) {
      var t;
      2 == e.button &&
        ((t = e.clientX),
        (e = e.clientY),
        INTERFACE.writeRightmenu(RMC_O, t, e),
        (screenmask_can_closed = !0));
    }),
    addEvent(configure, "click", function (e) {
      sort_def.className = "hide";
    }),
    addEvent(configure, "mouseup", function (e) {
      var t;
      2 == e.button &&
        ((t = e.clientX),
        (e = e.clientY),
        INTERFACE.writeRightmenu(RMC_S, t, e),
        (screenmask_can_closed = !0));
    }),
    addEvent(navigation, "mouseup", function (e) {
      var t;
      2 == e.button &&
        ((t = e.clientX),
        (e = e.clientY),
        INTERFACE.writeRightmenu(RMC_N, t, e),
        (screenmask_can_closed = !0));
    }),
    addEvent(workslist, "mouseup", function (e) {
      var t;
      2 == e.button &&
        ((t = e.clientX),
        (e = e.clientY),
        INTERFACE.writeRightmenu(RMC_W, t, e),
        (screenmask_can_closed = !0));
    }),
    addEvent(rightmenu, "click", function (e) {
      e = (e.target || e.srcElement).getAttribute("data-index");
      reg_num.test(e)
        ? INTERFACE.setUp(e)
        : ((input_tran = e != input_tran && e),
          CORE.translateSwitch(input_tran, !0),
          inwsM.focus()),
        (overmask.className = "overmask clear"),
        (screenmask_can_closed = !1);
    }),
    addEvent(lang_face, "click", function (e) {
      INTERFACE.pusMess(
        "Interface language unavailable temporarily!",
        "normal",
        6
      );
    }),
    addEvent(lang_tran, "mouseenter", function (e) {
      clearTimeout(hide_langtr_dy),
        clearTimeout(show_lang_tr_delay),
        (show_lang_tr_delay = setTimeout(function () {
          (lang_trch.className = "lang_trch show"),
            (searpoint.className = "searpoint hide"),
            clearTimeout(show_lang_tr_delay);
        }, 400));
    }),
    addEvent(language, "mouseleave", function (e) {
      clearTimeout(hide_langtr_dy),
        clearTimeout(show_lang_tr_delay),
        (hide_langtr_dy = setTimeout(function () {
          (lang_trch.className = "lang_trch hide"),
            (searpoint.className = "searpoint"),
            clearTimeout(hide_langtr_dy);
        }, 1e3));
    }),
    addEvent(lang_tran, "click", function (e) {
      (input_tran = !input_tran && LANG_IN[2].c),
        CORE.translateSwitch(input_tran, !0);
    }),
    addEvent(lang_trch, "click", function (e) {
      e = e.target || e.srcElement;
      (input_tran = e.getAttribute("data-index")),
        CORE.translateSwitch(input_tran, !0);
    }),
    addEvent(icon_clea, "click", function (e) {
      var t = UE_NERO.se[se_cu][ce_wn].u[sea_e].split("-");
      (engine_pure = hasClass(this, "point")
        ? (remClass(entryfield, "pure"),
          (icon_clea.className = "icon iconclea"),
          (t[3] = ""),
          !1)
        : (addClass(entryfield, "pure"),
          (icon_clea.className = "icon iconclea point"),
          (t[3] = "pure"),
          !0)),
        (UE_NERO.se[se_cu][ce_wn].u[sea_e] = t.join("-"));
    }),
    addEvent(icon_star, "click", function (e) {
      var t = UE_NERO.se[se_cu][ce_wn].u[sea_e].split("-"),
        a = getIat("engines", "ul")
          [ce_wn].getElementsByTagName("li")
          [sea_e].getElementsByTagName("label");
      (engine_star = hasClass(this, "point")
        ? (a[0].removeAttribute("class"),
          (icon_star.className = "icon iconstar"),
          (t[2] = ""),
          !1)
        : ((a[0].className = "star"),
          (icon_star.className = "icon iconstar point"),
          (t[2] = "star"),
          !0)),
        (UE_NERO.se[se_cu][ce_wn].u[sea_e] = t.join("-"));
    }),
    addEvent(inwsM, "click", function (e) {
      "" == inwsM.value.replace(reg_spc, "$1") &&
        UE_NERO.ue.c.his &&
        INPUTKEYTEXT.writehist();
    }),
    addEvent(inwsM, "keydown", function (e) {
      var t = e.keyCode;
      9 == t
        ? (INTERFACE.cancelDefault(e), INTERFACE.setUp(22))
        : e.ctrlKey && ((impls.className = "hover"), 65 == t) && inwsM.select();
    }),
    addEvent(inwsM, "input", function (e) {
      clearTimeout(keyword_cenkey_delay),
        (keyword_cenkey_delay = setTimeout(function () {
          inwsmInput(!1), clearTimeout(keyword_cenkey_delay);
        }, 200));
    }),
    addEvent(inwsM, "keyup", function (e) {
      inwsmInput(e);
    }),
    addEvent(implo, "click", function (e) {
      translate_notready || CORE.opnePage("major");
    }),
    addEvent(impls, "click", function (e) {
      translate_notready || CORE.opnePage("minor");
    }),
    addEvent(inwsmcan, "mouseenter", function (e) {
      INTERFACE.cutListSwitch(0), (keyboard_se = !1);
    }),
    addEvent(inwsmcan, "mouseleave", function (e) {
      keyboard_se = !0;
    }),
    addEvent(inwsmcan, "click", function (e) {
      e = e.target || e.srcElement;
      "clear" == e.id
        ? localStorage.removeItem("Aursear")
        : ((inwsM.value = e.innerText),
          input_tran && CORE.toTranslate(),
          CORE.censorKeyword(),
          INPUTKEYTEXT.getHint()),
        (inwsmcan.style.display = "none"),
        (keylists_se = 0);
    }),
    addEvent(inwstcan, "click", function (e) {
      e = e.target || e.srcElement;
      (inwsT.value = e.innerText), (inwstcan.style.display = "none");
    }),
    addEvent(copy_calend, "click", function (e) {
      var t = new Date().getTime();
      clearTimeout(airnotice_time_delay),
        clearInterval(refresh_time_timing),
        (refresh_times = 0),
        IMPORTANCE.getServicetime(t, "user"),
        (airnotice.className = "airnotice time ftf-tra"),
        (airnotice_time = !0);
    }),
    addEvent(copy_calend, "mouseleave", function (e) {
      clearTimeout(airnotice_time_delay),
        (airnotice_time_delay = setTimeout(function () {
          (airnotice.className = "airnotice note ftf-tra"),
            (airnotice_time = !1),
            clearTimeout(airnotice_time_delay);
        }, 3e3));
    }),
    addEvent(overmask, "click", function (e) {
      screenmask_can_closed &&
        ((overmask.className = "overmask clear"),
        (screenmask_can_closed = !1),
        UE_NERO.ue.x || inwsM.focus());
    }),
    addEvent(nav_find, "focus", function (e) {
      addClass(navselect, "hold"),
        addClass(navdetail, "hold"),
        (leftsidebar.style.opacity = "0.6"),
        (getIat("navselect", "label").className = "hide"),
        foc_lastnav_n && (foc_lastnav_n_last = foc_lastnav_n),
        (foc_lastnav_n = 0),
        (foc_lastnav_l = !1),
        (nav_find_blur = !1),
        NAVIGATE.focNavsite(!1);
    }),
    addEvent(nav_find, "keyup", function (t) {
      if (13 == t.keyCode) {
        t = nav_find.value
          .toLowerCase()
          .replace(reg_spc, "$1")
          .replace(reg_isp, "");
        let e;
        "" == t
          ? (nav_find.value = "")
          : ((find_end = NAVIGATE.setNavfind(t))
              ? ((navfid.innerHTML =
                  "<li>找到 " +
                  find_end +
                  ' 个</li><li class="forch">退出搜索<b>Esc</b></li>'),
                (navselect.className = "navselect findmar"),
                (navdetail.className = "navdetail findmar"),
                (RMC_N[1].a = !1))
              : ((e =
                  3 < t.length
                    ? "噗，啥都木有。关键词太长？"
                    : "噗，啥都木有。换个词试试！"),
                INTERFACE.pusMess(e, "normal", 4)),
            USERCONF.upAtion("NSW", t.replace(/\?|\&/g, "_"))),
          nav_find.blur();
      }
    }),
    addEvent(nav_find, "blur", function (e) {
      find_end ||
        (foc_lastnav_n_last &&
          ((foc_lastnav_n = foc_lastnav_n_last), (foc_lastnav_n_last = 0)),
        NAVIGATE.focNavsite(!0, !1)),
        "" == nav_find.value &&
          getIat("navselect", "label").removeAttribute("class"),
        remClass(navselect, "hold"),
        remClass(navdetail, "hold"),
        (leftsidebar.style.opacity = "1"),
        (nav_find_blur = !0);
    }),
    addEvent(selectlevel, "click", function (e) {
      (UE_NERO.ue.v.vw = 0), INTERFACE.setUp(16);
    }),
    addEvent(selectlabel, "click", function (e) {
      (UE_NERO.ue.v.vw = 1), INTERFACE.setUp(16);
    }),
    addEvent(navfid, "click", function (e) {
      getIat("navselect", "label").removeAttribute("class"),
        INTERFACE.setUp(16);
    }),
    addEvent(
      topengine,
      mousewheel,
      function (e) {
        e.wheelDelta
          ? (e.delta = -e.wheelDelta / 120)
          : (e.delta = (e.detail || 0) / 3),
          UE_NERO.ue.c.swi && 1 == e.delta && INTERFACE.setUp(6);
      },
      { passive: !0 }
    ),
    addEvent(
      navigation,
      mousewheel,
      function (e) {
        let t,
          a = rolling_factor[0],
          n =
            (mac && (a = rolling_factor[0] / 2),
            getId("navsite-" + del_lastnav_n));
        e.wheelDelta
          ? (e.delta = -e.wheelDelta / 120)
          : (e.delta = (e.detail || 0) / 3),
          (t = setInterval(function () {
            (a /= rolling_factor[2]),
              (n.scrollLeft += Math.ceil(e.delta * a)),
              Math.abs(a) < rolling_factor[1] && clearInterval(t);
          }, 16)),
          UE_NERO.ue.c.swi &&
            0 == n.scrollLeft &&
            -1 == e.delta &&
            INTERFACE.setUp(9);
      },
      { passive: !0 }
    ),
    addEvent(
      configure,
      mousewheel,
      function (e) {
        let t,
          a = rolling_factor[0] / 2;
        mac && (a = rolling_factor[0] / 4),
          e.wheelDelta
            ? (e.delta = -e.wheelDelta / 120)
            : (e.delta = (e.detail || 0) / 3),
          (t = setInterval(function () {
            (a /= rolling_factor[2]),
              (innersortlist.scrollTop += e.delta * a.toFixed(2)),
              Math.abs(a) < rolling_factor[1] && clearInterval(t);
          }, 16));
      },
      { passive: !0 }
    ),
    addEvent(
      innersort,
      "click",
      function (e) {
        if (
          null != (n = (e.target || e.srcElement).getAttribute("data-index"))
        ) {
          var t = getIat(innersort, "li"),
            a = t.length,
            n = ~~n;
          for (let e = 0; e < a; e++)
            n == e
              ? (verScroll(innersortlist, [0, 416, 1992][e]),
                (t[e].className = "active forc bacc_b"))
              : t[e].removeAttribute("class");
        }
      },
      { passive: !0 }
    ),
    addEvent(swindow, "click", function (e) {
      INTERFACE.cancelDefault(e);
    }),
    addEvent(swindow, "mousedown", function (e) {
      1 == e.button && INTERFACE.cancelDefault(e);
    }),
    addEvent(couclose, "click", function (e) {
      INTERFACE.setUp(61);
    }),
    addEvent(course_1, "click", function (e) {
      AUTO.moveCur(0, e.clientX, e.clientY, 200), AUTO.autoStep1();
    }),
    addEvent(course_2, "click", function (e) {
      AUTO.moveCur(0, e.clientX, e.clientY, 200), AUTO.autoStep2();
    }),
    addEvent(course_3, "click", function (e) {
      AUTO.moveCur(0, e.clientX, e.clientY, 200), AUTO.autoStep3();
    }),
    (String.prototype.toHex = function () {
      let e = this.toLowerCase();
      if (!reg_hex.test(e)) {
        if (reg_rgb.test(e) || reg_hsl.test(e)) {
          var n = (e = e.toRgb())
            .replace(/(?:\(|\)|rgb|a|\s+)*/g, "")
            .split(",");
          let a = "#";
          for (let t = 0; t < 3; t++) {
            let e = Number(n[t]).toString(16);
            n[t] < 16 && (e = "0" + e.toString()), (a += e);
          }
          return a;
        }
        return !1;
      }
      {
        var a = e.replace(/#/, "").split("");
        let t = "#";
        if (6 == a.length) return (t += a.join(""));
        if (3 == a.length) {
          for (let e = 0; e < 3; e++) t += a[e] + a[e];
          return t;
        }
      }
    }),
    (String.prototype.toRgb = function (t) {
      let a = this.toLowerCase();
      if (reg_hex.test(a)) {
        a = a.toHex();
        var n = [];
        for (let e = 1; e < 7; e += 2)
          n.push(parseInt("0x" + a.slice(e, e + 2)));
        return t
          ? ((c = t), "rgba(" + n.join(", ") + ", " + c + ")")
          : "rgb(" + n.join(", ") + ")";
      }
      if (reg_rgb.test(a)) {
        var s = a.replace(/(?:\(|\)|rgb|a|\s+)*/g, "").split(","),
          i = [];
        for (let t = 0; t < s.length; t++) {
          let e = Number(s[t]);
          0 <= t && t < 3
            ? ((e = Math.round(e)) < 0 ? (e = 0) : e, 255 < e ? (e = 255) : e)
            : 3 == t &&
              ((e = e.toFixed(2)) < 0 ? (e = 0) : e, 1 < e ? (e = 1) : e),
            (i[t] = e);
        }
        return 4 == s.length
          ? "rgba(" + i.join(", ") + ")"
          : "rgb(" + i.join(", ") + ")";
      }
      if (reg_hsl.test(a)) {
        var o,
          l,
          r,
          c = (a = a.toHsl()).replace(/(?:\(|\)|hsl|a|%|\s+)*/g, "").split(",");
        let e = [];
        return (
          (c[1] /= 100),
          (c[2] /= 100),
          (o =
            (r = (1 - Math.abs(2 * c[2] - 1)) * c[1]) *
            (1 - Math.abs(((c[0] / 60) % 2) - 1))),
          (l = c[2] - r / 2),
          0 <= c[0] && c[0] < 60
            ? (e = [r, o, 0])
            : 60 <= c[0] && c[0] < 120
            ? (e = [o, r, 0])
            : 120 <= c[0] && c[0] < 180
            ? (e = [0, r, o])
            : 180 <= c[0] && c[0] < 240
            ? (e = [0, o, r])
            : 240 <= c[0] && c[0] < 300
            ? (e = [o, 0, r])
            : 300 <= c[0] && c[0] < 360 && (e = [r, 0, o]),
          (e[0] = Math.round(255 * (e[0] + l))),
          (e[1] = Math.round(255 * (e[1] + l))),
          (e[2] = Math.round(255 * (e[2] + l))),
          t || c[3]
            ? ((r = t || c[3]), "rgba(" + e.join(", ") + ", " + r + ")")
            : "rgb(" + e.join(", ") + ")"
        );
      }
      return !1;
    }),
    (String.prototype.toHsl = function (e) {
      let t = this.toLowerCase();
      if (reg_hex.test(t) || reg_rgb.test(t)) {
        var a,
          n = (t = t.toRgb()).replace(/(?:\(|\)|rgb|a|\s+)*/g, "").split(","),
          s = [];
        if (
          ((n[0] /= 255),
          (n[1] /= 255),
          (n[2] /= 255),
          (o = Math.max(n[0], n[1], n[2])),
          (a = Math.min(n[0], n[1], n[2])),
          (s[2] = (o + a) / 2),
          o == a)
        )
          s[0] = s[1] = 0;
        else {
          var i = o - a;
          switch (((s[1] = 0.5 < s[0] ? i / (2 - o - a) : i / (o + a)), o)) {
            case n[0]:
              s[0] = (n[1] - n[2]) / i + (n[1] < n[2] ? 6 : 0);
              break;
            case n[1]:
              s[0] = (n[2] - n[0]) / i + 2;
              break;
            case n[2]:
              s[0] = (n[0] - n[1]) / i + 4;
          }
          s[0] /= 6;
        }
        return (
          (s[0] = Math.round(360 * s[0])),
          (s[1] = Math.round(100 * s[1]) + "%"),
          (s[2] = Math.round(100 * s[2]) + "%"),
          e || n[3]
            ? ((a = e || n[3]), "hsla(" + s.join(", ") + ", " + a + ")")
            : "hsl(" + s.join(", ") + ")"
        );
      }
      if (reg_hsl.test(t)) {
        var o,
          l = t.replace(/(?:\(|\)|hsl|a|%|\s+)*/g, "").split(","),
          r = [];
        for (let t = 0; t < 3; t++) {
          let e = Number(l[t]);
          0 == t
            ? 0 < (e = Math.round(e)) && e < 360
              ? e
              : (e = 0)
            : 0 < t && t < 3
            ? ((e = Math.round(e)) < 0 ? (e = 0) : e,
              100 < e ? (e = 100) : e,
              (e += "%"))
            : 3 == t &&
              ((e = e.toFixed(2)) < 0 ? (e = 0) : e, 1 < e ? (e = 1) : e),
            (r[t] = e);
        }
        return e || l[3]
          ? ((o = e || l[3]), "hsla(" + r.join(", ") + ", " + o + ")")
          : "hsl(" + r.join(", ") + ")";
      }
      return !1;
    });
  const IMPORTANCE = {
      getServicetime: function (e, t) {
        var a = localStorage.getItem("Aurmeet");
        let n = JSON.parse(a) || [],
          s = new XMLHttpRequest();
        (s.onreadystatechange = function () {
          var e;
          4 == s.readyState && 200 == s.status
            ? ((e = s.getResponseHeader("Date")),
              (e = new Date(e)),
              (n[0] = e.getTime()),
              localStorage.setItem("Aurmeet", JSON.stringify(n)),
              SOLARLUNAR.writecalendar(e, t))
            : 404 == s.status &&
              location.replace("https://%61%75%72%2E%6F%6E%65/");
        }),
          s.open("GET", INITURL.i + OST, !0),
          s.setRequestHeader("If-Modified-Since", "0"),
          s.send();
      },
      userVisit: function (t) {
        var e = INITURL[t.charAt(UP++)];
        let a = new XMLHttpRequest();
        (a.onreadystatechange = function () {
          if (4 == a.readyState && 200 == a.status) {
            const e = JSON.parse(a.responseText);
            console.log(e.agreement);
            IMPORTANCE.parsAgree(e.agreement, 0, t);
            IMPORTANCE.parsAgree(e.hotkey, 2, t);
            IMPORTANCE.parsAgree(e.callout, 1, t);
            IMPORTANCE.parsAgree(e.donate, 3, t);
          } else if (4 == a.readyState && 404 == a.status) {
            location.replace("https://%61%75%72%2E%6F%6E%65/");
          }
        }),
          a.open("GET", e, !0),
          a.send();
      },
      parsAgree: function (s, i, o) {
        var l = s.text,
          r = l.length;
        let c = document.createElement("div");
        var u = document.createElement("h4");
        let d = document.createElement("div");
        var a,
          e,
          m = document.createElement("div"),
          n = document.createElement("ul"),
          _ = document.createElement("ul"),
          E = document.createElement("span"),
          f = document.createElement("p"),
          p = getId("agreement");
        if (
          (p && !i && (w = p.parentNode) && w.removeChild(p),
          (c.className = "total"),
          (u.className = "title"),
          (d.className = "conte"),
          firefox && (d.className = "conte firefox"),
          (m.className = "sure forch"),
          (u.innerHTML = s.title
            .replace(/\[/g, "<span>")
            .replace(/\]/g, "</span>")),
          (n.className = "left"),
          (_.className = "righ"),
          (E.innerHTML = "<i>\\</i>" + s.entr),
          (m.innerText = "确定"),
          i)
        )
          if (1 == i) {
            for (let t = 0; t < r; t++) {
              let e;
              l[t].h
                ? ((a = document.createElement("h6")),
                  l[t].s && (a.className = l[t].s),
                  (a.innerText = l[t].h),
                  d.appendChild(a))
                : l[t].p &&
                  ((a = document.createElement("p")),
                  l[t].s && (a.className = l[t].s),
                  (e = (e = (e = l[t].p).replace(/\[/g, "<span>")).replace(
                    /\]/g,
                    "</span>"
                  )),
                  (a.innerHTML = e),
                  d.appendChild(a));
            }
            c.appendChild(u),
              c.appendChild(d),
              c.appendChild(m),
              (SW_HTML[i] = c),
              addEvent(sort_coprd, "click", function (e) {
                (swindow.innerHTML = ""),
                  swindow.appendChild(SW_HTML[1]),
                  (overmask.className = "overmask show"),
                  (screenmask_can_closed = !0);
              }),
              addEvent(m, "click", function () {
                (overmask.className = "overmask clear"),
                  UE_NERO.ue.x || inwsM.focus();
              });
          } else if (2 == i) {
            (E.id = "shortcut"), (E.className = "shortcut");
            for (var T = 0; T < r; T++) {
              let e, t;
              var g = document.createElement("li"),
                v = document.createElement("li");
              l[T].t
                ? ((t = l[T].c),
                  (g.className = "title"),
                  (v.className = "title"),
                  mac &&
                    (t = (t = t.replace(/Ctrl/g, "⌘")).replace(/Alt/g, "⎇")),
                  (g.innerText = l[T].t),
                  (v.innerText = t))
                : ((e = (e = (e = (e = (e = l[T].n).replace(
                    /\//g,
                    "&nbsp;/&nbsp;"
                  )).replace(/\[/g, "<b>")).replace(/\]/g, "</b>")).replace(
                    /\*/g,
                    "/"
                  )),
                  mac &&
                    (e = (e = e.replace(/Ctrl/g, "Command")).replace(
                      /Alt/g,
                      "Option"
                    )),
                  l[T].o &&
                    ((g.className = "copy"),
                    g.setAttribute("data-index", l[T].o)),
                  (g.innerHTML = e),
                  (v.innerHTML = l[T].c.replace(/\//g, "&nbsp;/&nbsp;"))),
                l[T].s && addClass(g, l[T].s),
                n.appendChild(g),
                _.appendChild(v),
                addEvent(g, "click", function () {
                  copyText(
                    (event.target || event.srcElement).getAttribute(
                      "data-index"
                    )
                  )
                    ? INTERFACE.pusMess("链接已复制到剪贴板！", "carry", 4)
                    : INTERFACE.pusMess("复制失败", "fail", 4);
                });
            }
            d.appendChild(n),
              d.appendChild(_),
              c.appendChild(u),
              c.appendChild(d),
              c.appendChild(m),
              (SW_HTML[i] = c),
              copyright.appendChild(E),
              addEvent(E, "click", function () {
                (swindow.innerHTML = ""),
                  swindow.appendChild(SW_HTML[2]),
                  (overmask.className = "overmask show"),
                  (screenmask_can_closed = !0),
                  setTimeout(function () {
                    inwsM.blur();
                  }, 200);
              }),
              addEvent(m, "click", function () {
                (overmask.className = "overmask clear"),
                  UE_NERO.ue.x || inwsM.focus();
              });
          } else
            3 == i &&
              ((f.className = "sent_don"),
              (f.innerText = s.entr),
              (w = document.createElement("i")),
              (p = document.createElement("i")),
              (e = document.createElement("p")),
              (w.className = "donate left"),
              (p.className = "donate righ"),
              firefox &&
                ((w.className = "donate firefox left"),
                (p.className = "donate firefox righ")),
              (e.className = "legend"),
              (e.innerText = s.legend),
              (c.className = "total less"),
              d.appendChild(w),
              d.appendChild(p),
              d.appendChild(e),
              c.appendChild(u),
              c.appendChild(d),
              c.appendChild(m),
              (SW_HTML[i] = c),
              sort_sentences.insertBefore(f, sort_sentences.childNodes[0]),
              addEvent(f, "click", function () {
                (swindow.innerHTML = ""),
                  swindow.appendChild(SW_HTML[3]),
                  (overmask.className = "overmask show"),
                  (screenmask_can_closed = !0),
                  setTimeout(function () {
                    inwsM.blur();
                  }, 200),
                  USERCONF.upAtion("SEL", "donate");
              }),
              addEvent(m, "click", function () {
                (overmask.className = "overmask clear"),
                  UE_NERO.ue.x || inwsM.focus();
              }));
        else {
          let e, t;
          var h,
            N,
            w = localStorage.getItem("Aurmeet"),
            p = JSON.parse(w) || [];
          let a = document.createElement("div"),
            n = document.createElement("div");
          (e = s.tance),
            (t = s.ver),
            (E.id = "agreement"),
            (E.className = "agreement"),
            (a.className = "refuse forch"),
            (n.className = "agree forch"),
            (f.className = "sent_use"),
            (f.innerText = s.entr);
          for (let e = 0; e < r; e++)
            l[e].h
              ? ((h = document.createElement("h6")),
                l[e].s && (h.className = l[e].s),
                (h.innerText = l[e].h),
                d.appendChild(h))
              : l[e].p
              ? ((h = document.createElement("p")),
                l[e].s && (h.className = l[e].s),
                (h.innerText = l[e].p),
                d.appendChild(h))
              : l[e].a &&
                (((N = document.createElement("a")).className = "forch"),
                l[e].s && (N.className = "forch " + l[e].s),
                l[e].l
                  ? N.setAttribute("data-index", l[e].l)
                  : l[e].d &&
                    (N.setAttribute("data-index", l[e].d),
                    N.setAttribute("download", l[e].d.split("/").slice(-1))),
                (N.innerText = l[e].a),
                d.appendChild(N),
                addEvent(N, "click", function (e) {
                  var e = e.target || e.srcElement,
                    t = e.getAttribute("data-index"),
                    e = e.getAttribute("download");
                  t && e ? downFile(t, e) : t && window.open(t, openw[2]);
                }));
          c.appendChild(u),
            c.appendChild(d),
            0 != p.length
              ? "ALL-R" == e || ("NEW-R" == e && p[3] != t)
                ? (copyright.appendChild(E), IMPORTANCE.getemit(t, o))
                : "ALL-D" == e || ("NEW-D" == e && p[3] != t)
                ? ((overload.className = "overload"),
                  (swindow.innerHTML = ""),
                  (a.innerText = s.refuse),
                  (n.innerText = s.agree),
                  c.appendChild(a),
                  c.appendChild(n),
                  swindow.appendChild(c),
                  (overmask.className = "overmask show"))
                : IMPORTANCE.getemit(t, o)
              : "ALL-R" == e || "FIR-R" == e || "NEW-R" == e
              ? (copyright.appendChild(E), IMPORTANCE.getemit(t, o))
              : "ALL-D" == e || "FIR-D" == e || "NEW-D" == e
              ? ((overload.className = "overload"),
                (swindow.innerHTML = ""),
                (a.innerText = s.refuse),
                (n.innerText = s.agree),
                c.appendChild(a),
                c.appendChild(n),
                swindow.appendChild(c),
                (overmask.className = "overmask show"))
              : IMPORTANCE.getemit(t, o),
            addEvent(a, "click", function (e) {
              window.open("about:blank", "_self").close();
            }),
            addEvent(n, "click", function (e) {
              c.removeChild(a),
                c.removeChild(n),
                (overmask.className = "overmask clear"),
                IMPORTANCE.getemit(t, o);
            }),
            addEvent(m, "click", function () {
              (overmask.className = "overmask clear"),
                UE_NERO.ue.x || inwsM.focus();
            }),
            addEvent(E, "click", function () {
              (swindow.innerHTML = ""),
                swindow.appendChild(SW_HTML[0]),
                (overmask.className = "overmask show"),
                setTimeout(function () {
                  inwsM.blur();
                }, 200);
            }),
            addEvent(f, "click", function (e) {
              (swindow.innerHTML = ""),
                swindow.appendChild(SW_HTML[0]),
                (overmask.className = "overmask show"),
                (screenmask_can_closed = !0);
            }),
            c.appendChild(m),
            (SW_HTML[i] = c);
        }
        addEvent(
          overmask,
          mousewheel,
          function (e) {
            let t,
              a = rolling_factor[0] / 2;
            mac && (a = rolling_factor[0] / 4),
              e.wheelDelta
                ? (e.delta = -e.wheelDelta / 120)
                : (e.delta = (e.detail || 0) / 3),
              (t = setInterval(function () {
                (a /= rolling_factor[2]),
                  (d.scrollTop += e.delta * a.toFixed(2)),
                  Math.abs(a) < rolling_factor[1] && clearInterval(t);
              }, 16));
          },
          { passive: !0 }
        );
      },
      getemit: function (e, t) {
        let a = new XMLHttpRequest();
        (a.onreadystatechange = function () {
          4 == a.readyState &&
            200 == a.status &&
            ((SO_NERO = JSON.parse(a.responseText)), USERCONF.read(e, t));
        }),
          a.open("GET", INITURL[t.charAt(UP++)], !0),
          a.send();
      },
    },
    USERCONF = {
      read: function (e, t) {
        let a = !1,
          n = localStorage.getItem("Aurconf");
        var s = localStorage.getItem("Aurmeet");
        let i = JSON.parse(s) || [],
          o = new XMLHttpRequest();
        (o.onreadystatechange = function () {
          4 == o.readyState &&
            200 == o.status &&
            ((UE_NERO = JSON.parse(o.responseText)),
            n &&
              ((UE_NERO.ue = JSON.parse(n)),
              "办公" != UE_NERO.ue.w[2].c && (UE_NERO.ue.w[2].c = "办公"),
              "number" != typeof UE_NERO.ue.c.act && (UE_NERO.ue.c.act = 2),
              "number" != typeof UE_NERO.ue.v.vw && (UE_NERO.ue.v.vw = 0),
              (!UE_NERO.ue.v.ve || i[1] < now_ver) &&
                (UE_NERO.ue.v.ve = ["生活", "品牌", "服务"]),
              (UE_NERO.ue.o && !Array.isArray(UE_NERO.ue.o)) ||
                (UE_NERO.ue.o = "0100"),
              UE_NERO.ue.SE_N ||
                (UE_NERO.ue.SE_N = [
                  [2, 0, 0, 1, 5, 2, 2],
                  [0, 1, 1, 0, 1, 0, 0, 0],
                  [0, 0, 0, 0, 0, 0, 0],
                  [0, 1, 1, 5, 1, 0, 0],
                ]),
              UE_NERO.ue.c.ope || (UE_NERO.ue.c.ope = [0, 0, 0])),
            i[1]
              ? (i[1] < now_ver
                  ? UE_NERO.ue.c.pre && (a = AUTO.engLoca(UE_NERO.ue.o))
                  : (UE_NERO.ue.c.pre && (a = AUTO.engLoca(UE_NERO.ue.o)),
                    UE_NERO.ue.SE_N ||
                      (UE_NERO.ue.SE_N = [
                        [2, 0, 0, 1, 5, 2, 2],
                        [0, 1, 1, 0, 1, 0, 0, 0],
                        [0, 0, 0, 0, 0, 0, 0],
                        [0, 1, 1, 5, 1, 0, 0],
                      ])),
                USERCONF.toplate(a),
                INSTALLSET.utilize(!1, t))
              : (1680 < screen_size[0] && (UE_NERO.ue.t = 1),
                (UE_NERO.ue.SE_N = [
                  [2, 0, 0, 1, 5, 2, 2],
                  [0, 1, 1, 0, 1, 0, 0, 0],
                  [0, 0, 0, 0, 0, 0, 0],
                  [0, 1, 1, 5, 1, 0, 0],
                ]),
                USERCONF.toplate(),
                INSTALLSET.utilize("first", t)),
            (i[1] = now_ver),
            (i[2] && "" != i[2] && 20180101 != i[2]) ||
              (i[2] = Math.random().toString(16).substring(2, 14)),
            (i[3] = ~~e),
            localStorage.setItem("Aurmeet", JSON.stringify(i)));
        }),
          o.open("GET", INITURL[t.charAt(UP++)], !0),
          o.send();
        console.log(UP);
      },
      toplate: function (e) {
        var t = UE_NERO.ue.w.length;
        for (let e = 0; e < LANG_IN.length; e++) {
          var a = document.createElement("li");
          a.setAttribute("data-index", LANG_IN[e].c),
            (a.innerText = LANG_IN[e].n),
            lang_trch.appendChild(a);
        }
        e && (UE_NERO.ue.n = e[0]),
          (!reg_num.test(UE_NERO.ue.n) || UE_NERO.ue.n > t) &&
            (UE_NERO.ue.n = 0),
          (se_cu = UE_NERO.ue.n),
          (searpoint.innerHTML = "");
        for (let e = 0; e < t; e++) {
          var n = document.createElement("li");
          (n.innerText = UE_NERO.ue.w[e].c),
            n.setAttribute("data-index", e),
            e == se_cu && (n.className = "active"),
            2 == e &&
              n.setAttribute("style", "opacity: 0.4; pointer-events: none;"),
            searpoint.appendChild(n);
        }
        addEvent(searpoint, "click", function (e) {
          var e = ~~(e.target || e.srcElement).getAttribute("data-index"),
            t = getIat("searpoint", "li"),
            a = t.length;
          if (e <= a) {
            (UE_NERO.ue.n = e), (se_cu = UE_NERO.ue.n);
            for (let e = 0; e < a; e++)
              e == se_cu
                ? (t[e].className = "active")
                : t[e].removeAttribute("class");
            USERCONF.write();
          }
        }),
          this.write(e);
      },
      write: function (e) {
        var a = SO_NERO.length,
          t = [],
          n = [],
          s = UE_NERO.se[se_cu];
        let i = s.length;
        (ce_co = []), (ins_w = []), (NEW_SO = []);
        for (let e = 0; e < i; e++) {
          var o = [],
            l = [],
            r = s[e].c,
            c = s[e].u,
            u = s[e].o,
            d = c.length;
          for (let t = 0; t < d; t++)
            for (let e = 0; e < a; e++) {
              var m = SO_NERO[e].u.split("-"),
                _ = c[t].split("-");
              if (m[0] == _[0]) {
                "star" == _[2] && (SO_NERO[e].star = !0),
                  o.push(SO_NERO[e]),
                  l.push(c[t]);
                break;
              }
              INSPECT.offline(c[t]);
            }
          0 == o.length
            ? (UE_NERO.se[se_cu].splice(e, 1), e--, i--)
            : (UE_NERO.se[se_cu][e].n >= o.length &&
                (UE_NERO.se[se_cu][e].n = 0),
              (UE_NERO.se[se_cu][e].u = l),
              ce_co.push(u),
              NEW_SO.push(o),
              n.push(r));
        }
        (t = UE_NERO.ue.SE_N[se_cu]),
          e && (UE_NERO.ue.w[se_cu].n = e[1]),
          !reg_num.test(UE_NERO.ue.w[se_cu].n) ||
          UE_NERO.ue.w[se_cu].n >= n.length
            ? (UE_NERO.ue.w[se_cu].n = 0)
            : (ce_wn = UE_NERO.ue.w[se_cu].n),
          e && (t[ce_wn] = e[2]),
          WRITEEMERGE.inengines(NEW_SO, t),
          WRITEEMERGE.infication(n);
      },
      saveUSERUE: function (e) {
        let t = 2e3;
        clearTimeout(auto_save_delay),
          e && (t = 100),
          (auto_save_delay = setTimeout(function () {
            localStorage.setItem("Aurconf", JSON.stringify(UE_NERO.ue)),
              clearTimeout(auto_save_delay);
          }, t));
      },
      upAtion: function (e, t, a) {
        var n = localStorage.getItem("Aurmeet");
        let s = "u=" + JSON.parse(n)[2] + "&v=" + now_ver + "&t=" + loa_sta,
          i;
        e && (s += "&w=" + e),
          t && (s += "&c=" + t),
          a && (s += "&m=" + a),
          ((i = new XMLHttpRequest()).onreadystatechange = function () {
            4 == i.readyState && i.status;
          }),
          i.open("POST", "note/carr/ation.php", !0),
          i.setRequestHeader(
            "Content-type",
            "application/x-www-form-urlencoded"
          ),
          i.send(s);
      },
    },
    WRITEEMERGE = {
      infication: function (t) {
        let a = t.length;
        fication.innerHTML = "";
        for (let e = 0; e < a; e++) {
          var n = document.createElement("li");
          (n.innerText = t[e]),
            n.setAttribute("data-index", e),
            fication.appendChild(n),
            (ins_w[e] = n.clientWidth),
            addEvent(n, "mouseenter", function (e) {
              e = ~~(e.target || e.srcElement).getAttribute("data-index");
              e <= a && CORE.switchShuttle(e);
            }),
            addEvent(n, "click", function (e) {
              e = ~~(e.target || e.srcElement).getAttribute("data-index");
              e <= a && CORE.switchFication(e);
            });
        }
        CORE.switchFication(ce_wn),
          addEvent(fication, "mouseleave", function (e) {
            CORE.switchShuttle(ce_wn);
          });
      },
      inengines: function (e, n) {
        var t = e.length;
        engines.innerHTML = "";
        for (let a = 0; a < t; a++) {
          var s = e[a],
            i = s.length,
            o = document.createElement("ul");
          for (let t = 0; t < i; t++) {
            var l = document.createElement("li"),
              r = document.createElement("input"),
              c = document.createElement("label"),
              u = document.createElement("span"),
              d = ["normal", "free", "toll", "risk"],
              m = s[t].z;
            let e = s[t].t;
            -1 != e.indexOf("/") && (e = s[t].t.split("/")[0]),
              (r.type = "radio"),
              (r.id = "engine" + a + t),
              (r.name = "engine" + a),
              n[a] == t && (r.checked = "checked"),
              c.setAttribute("for", "engine" + a + t),
              c.setAttribute("data-index", t),
              UE_NERO.ue.c.cop && m && addClass(c, d[m]),
              s[t].star && addClass(c, "star"),
              i < 4
                ? ((c.innerText = s[t].c), (u.innerText = e))
                : ((c.innerText = e), (u.innerText = s[t].c)),
              (u.className = "ftf-nat forc"),
              l.appendChild(r),
              c.appendChild(u),
              l.appendChild(c),
              o.appendChild(l),
              addEvent(c, "click", function (e) {
                e = ~~(e.target || e.srcElement).getAttribute("data-index");
                CORE.switchEngine(e);
              }),
              addEvent(c, "dblclick", function (e) {
                CORE.opnePage("just");
              });
          }
          engines.appendChild(o);
        }
      },
    },
    CORE = {
      switchShuttle: function (t) {
        let a = 0;
        for (let e = 0; e < ins_w.length; e++) e < t && (a += ins_w[e]);
        (shuttle.style.webkitTransform = "translate3d(" + a + "px, 0, 0)"),
          (shuttle.style.mozTransform = "translate3d(" + a + "px, 0, 0)"),
          (shuttle.style.msTransform = "translate3d(" + a + "px, 0, 0)"),
          (shuttle.style.oTransform = "translate3d(" + a + "px, 0, 0)"),
          (shuttle.style.transform = "translate3d(" + a + "px, 0, 0)"),
          (shuttle.style.width = ins_w[t] + "px"),
          UE_NERO.ue.c.col ||
            ((now_co = ce_co[t]), (shuttle.style.backgroundColor = now_co));
      },
      switchFication: function (e) {
        var t = getIat("fication", "li"),
          a = getIat("engines", "ul");
        (UE_NERO.ue.w[se_cu].n = e), (ce_wn = UE_NERO.ue.w[se_cu].n);
        for (let e = 0; e < t.length; e++)
          e < ce_wn
            ? (t[e].removeAttribute("class"), (a[e].className = "last"))
            : e == ce_wn
            ? ((t[e].className = "active"), (a[e].className = "now"))
            : (t[e].removeAttribute("class"), (a[e].className = "next"));
        this.switchShuttle(ce_wn),
          this.switchEngine(UE_NERO.ue.SE_N[se_cu][ce_wn]),
          UE_NERO.ue.c.col ||
            ((now_co = ce_co[ce_wn]), INTERFACE.cutTheme(now_co));
      },
      switchEngine: function (e) {
        var t = getIat("engines", "ul")[ce_wn].getElementsByTagName("li"),
          a = UE_NERO.se[se_cu][ce_wn].u[e].split("-");
        (sea_e = e), (UE_NERO.ue.SE_N[se_cu][ce_wn] = sea_e);
        for (let e = 0; e < t.length; e++)
          if (e == sea_e) {
            t[e].childNodes[0].checked = !0;
            break;
          }
        (NOW_SO = NEW_SO[ce_wn][sea_e]),
          (input_tran = a[1]),
          (engine_star = a[2]),
          (engine_pure = a[3]),
          NOW_SO.u.split("-")[1]
            ? ((aimlgg.innerText = NOW_SO.u.split("-")[1]),
              (aimlgg.style.opacity = "1"))
            : (aimlgg.style.opacity = "0"),
          NOW_SO.o
            ? ((inwsMla.innerText = NOW_SO.o + ":"),
              (inwsMla.style.opacity = "1"),
              (inwsM.style.paddingLeft = 56 + inwsMla.clientWidth + "px"))
            : ((inwsMla.style.opacity = "0"),
              (inwsM.style.paddingLeft = "56px")),
          input_tran
            ? CORE.translateSwitch(input_tran, !1)
            : CORE.translateSwitch(!1, !1),
          engine_star
            ? (icon_star.className = "icon iconstar point")
            : (icon_star.className = "icon iconstar"),
          NOW_SO.r
            ? ((icon_clea.style.opacity = "1"),
              (icon_clea.style.pointerEvents = "all"))
            : ((icon_clea.style.opacity = "0"),
              (icon_clea.style.pointerEvents = "none")),
          engine_pure
            ? (addClass(entryfield, "pure"),
              (icon_clea.className = "icon iconclea point"))
            : (remClass(entryfield, "pure"),
              (icon_clea.className = "icon iconclea")),
          USERCONF.saveUSERUE(),
          addClass(entryfield, "hover"),
          clearTimeout(blinking_entryfield_delay),
          (blinking_entryfield_delay = setTimeout(function () {
            remClass(entryfield, "hover"),
              clearTimeout(blinking_entryfield_delay);
          }, 200)),
          NOW_SO.u.split("-")[0] == UE_NERO.ue.o
            ? (RMC_E[1].a = !1)
            : (RMC_E[1].a = !0),
          this.censorKeyword();
      },
      censorKeyword: function () {
        var n = inwsM.value.toLowerCase(),
          t = n
            .replace(reg_spc, "$1")
            .replace(reg_isp, " ")
            .replace(
              /(^.*?)|官方|首页|主页|网站|官网|O\.W\.|Official Website*$/gi,
              "$1"
            );
        if ("" == t)
          this.searchStatus("官网", 0),
            (official_iurl = NOW_SO.i),
            (inwsT.value = ""),
            (RMC_E[0].a = !1),
            (RMC_E[2].a = !1),
            input_tran && (inwsTla.style.opacity = "1"),
            INPUTKEYTEXT.getHint();
        else if (NOW_SO.e && /(^http|^https):\/\/(\w+)\.(\w+\.*\w*)/.test(t))
          (official_iurl = t),
            this.searchStatus("打开", 2),
            (RMC_E[0].a = !0),
            (RMC_E[2].a = !0);
        else if (NOW_SO.e) {
          var e = SO_NERO.length;
          e: for (let a = 0; a < e; a++) {
            var s,
              i = SO_NERO[a].t.split("/"),
              o = i.length;
            for (let e = 0; e < o; e++) {
              if (t == i[e].toLowerCase()) {
                if (SO_NERO[a].c) {
                  let t = 0;
                  var l = n.length;
                  for (let e = 0; e < l; e++)
                    0 != (65280 & n.charCodeAt(e)) && t++, t++;
                  (s = 80 + 13 * t),
                    (slogan.innerText = SO_NERO[a].c),
                    (slogan.style.left = s + "px");
                }
                (official_iurl = SO_NERO[a].i), this.searchStatus("官网", 1);
                break e;
              }
              this.searchStatus("搜索", 0), (official_iurl = !1);
            }
          }
          (RMC_E[0].a = !0), (RMC_E[2].a = !0);
        } else
          NOW_SO.v
            ? (this.searchStatus(NOW_SO.v, 0), (official_iurl = !1))
            : (this.searchStatus("搜索", 0),
              (official_iurl = !1),
              (RMC_E[0].a = !0),
              (RMC_E[2].a = !0));
      },
      searchStatus: function (e, t) {
        last_button_less == t ||
          input_tran ||
          (1 == t
            ? (entryfield.className = "entryfield home")
            : 2 == t
            ? (entryfield.className = "entryfield link")
            : ((entryfield.className = "entryfield sear"),
              engine_pure && addClass(entryfield, "pure"))),
          last_button_firs != e &&
            ((implo.innerHTML = e + "<b><i>Enter</i></b>"),
            (implo.className = "hover"),
            clearTimeout(blinking_implo_delay),
            (blinking_implo_delay = setTimeout(function () {
              implo.removeAttribute("class"),
                clearTimeout(blinking_implo_delay);
            }, 200))),
          (last_button_less = t),
          (last_button_firs = e);
      },
      toTranslate: function () {
        let e = input_tran.toLowerCase();
        (implement.className = "implement notready ftf-tra"),
          (translate_notready = !0),
          clearTimeout(keyword_gettra_delay),
          (keyword_gettra_delay = setTimeout(function () {
            CORE.getTranslateBd("auto", e),
              CORE.getTranslateYd("auto", e),
              clearTimeout(keyword_gettra_delay);
          }, keyword_gettra_time));
      },
      translateSwitch: function (t, e) {
        var a = inwsM.value.replace(reg_spc, "$1").replace(reg_isp, " "),
          n = getIat("lang_trch", "li"),
          s = n.length;
        if (t)
          for (let e = 0; e < s; e++)
            t == LANG_IN[e].c
              ? ((n[e].className = "active"),
                (lang_tran.className = "lang_tran active"),
                (entryfield.className = "entryfield tran"),
                (input_tran_n = LANG_IN[e].n),
                (inwsT.value = ""),
                (inwsTla.innerText = "翻译为" + input_tran_n),
                "" != a && CORE.toTranslate())
              : n[e].removeAttribute("class");
        else {
          for (let e = 0; e < s; e++) n[e].removeAttribute("class");
          (lang_tran.className = "lang_tran"),
            (inwstcan.style.display = "none"),
            (entryfield.className = "entryfield sear"),
            (inwsT.value = "");
        }
        e &&
          (((e = UE_NERO.se[se_cu][ce_wn].u[sea_e].split("-"))[1] = t || ""),
          (UE_NERO.se[se_cu][ce_wn].u[sea_e] = e.join("-")),
          CORE.censorKeyword());
      },
      getTranslateBd: function (e, t) {
        let a;
        var n = inwsM.value.replace(reg_spc, "$1").replace(reg_isp, " "),
          n =
            "q=" +
            encodeURI(n) +
            ("&from=" + (e || "auto")) +
            ("&to=" + (t || input_tran.toLowerCase())) +
            ("&salt=" + loa_sta) +
            "&line=1";
        ((a = new XMLHttpRequest()).onreadystatechange = function () {
          var e;
          4 == a.readyState &&
            200 == a.status &&
            ((e = JSON.parse(a.responseText)), CORE.writeTranslateBd(e));
        }),
          a.open("POST", INITURL.e, !0),
          a.setRequestHeader(
            "Content-type",
            "application/x-www-form-urlencoded"
          ),
          a.send(n);
      },
      getTranslateYd: function (e, t) {
        let a;
        let n;
        var s = inwsM.value.replace(reg_spc, "$1").replace(reg_isp, " "),
          s = "q=" + encodeURI(s),
          e = "&from=" + (e || "auto");
        if ("cht" == (a = t || input_tran.toLowerCase())) return !1;
        (t =
          s +
          e +
          ("&to=" +
            (a = (a = (a = (a = a.replace(/zh/g, "zh-CHS")).replace(
              /jp/g,
              "ja"
            )).replace(/kor/g, "ko")).replace(/fra/g, "fr"))) +
          ("&salt=" + loa_sta) +
          "&line=2"),
          ((n = new XMLHttpRequest()).onreadystatechange = function () {
            var e;
            4 == n.readyState &&
              200 == n.status &&
              ((e = JSON.parse(n.responseText)), CORE.writeTranslateYd(e));
          }),
          n.open("POST", INITURL.e, !0),
          n.setRequestHeader(
            "Content-type",
            "application/x-www-form-urlencoded"
          ),
          n.send(t);
      },
      writeTranslateBd: function (e) {
        if (translate_notready && e.trans_result) {
          var t = e.trans_result,
            a = [],
            n = t.length;
          if (((inwstcan.style.display = "none"), 1 == n))
            inwsT.value = t[0].dst;
          else if (1 < n) {
            inwsT.value = t[0].dst;
            for (let e = 1; e < n && !(e > hints_rows); e++)
              a[e] = '<li class="bacch_b">' + t[e].dst + "</li>";
            (inwstcan.innerHTML =
              '<ul id="keylist" class="keylist">' + a.join("") + "</ul>"),
              (inwstcan.style.display = "block");
          }
          (implement.className = "implement ftf-tra"),
            (translate_notready = !1),
            (inwsTla.style.opacity = "0");
        }
      },
      writeTranslateYd: function (t) {
        if (translate_notready && t.translation) {
          var n = t.translation,
            s = [];
          let a = 0;
          var i = t.web;
          if (!i) return !1;
          var o = i.length;
          let e;
          (inwstcan.style.display = "none"),
            (e =
              2 == n[0].split(/\s+/).length
                ? n[0].replace(/^(the|a)\s/gi, "")
                : n[0]),
            (inwsT.value = e);
          for (let t = 0; t < o; t++) {
            let e = i[t].value;
            var l = e.length;
            if (reg_chi.test(e[0])) {
              if (((e = i[t].key), ++a > hints_rows)) break;
              s[a] = '<li class="bacch_b">' + e + "</li>";
            } else
              for (var r = 0; r < l && !(++a > hints_rows); r++)
                s[a] = '<li class="bacch_b">' + e[r] + "</li>";
          }
          (inwstcan.innerHTML =
            '<ul id="keylist" class="keylist">' + s.join("") + "</ul>"),
            (inwstcan.style.display = "block"),
            (implement.className = "implement ftf-tra"),
            (translate_notready = !1),
            (inwsTla.style.opacity = "0");
        }
      },
      opnePage: function (e) {
        let t, a;
        var n = inwsM.value.replace(reg_spc, "$1").replace(reg_isp, " "),
          s = encodeURIComponent(n),
          i = inwsT.value.replace(reg_spc, "$1").replace(reg_isp, " "),
          i = encodeURIComponent(i);
        (t = (engine_pure ? NOW_SO.r : NOW_SO.s).split("*")),
          (a =
            "just" == e
              ? NOW_SO.i
              : "major" == e && official_iurl
              ? official_iurl
              : input_tran
              ? t[0] + i + t[2]
              : "py" == t[1]
              ? (e = INPUTKEYTEXT.convertPinyin(n))
                ? t[0] + e + t[2]
                : NOW_SO.i
              : t[0] + s + t[2]),
          window.open(a, openw[0]),
          USERCONF.upAtion("SOA", a.replace(/\?|\&/g, "_"), openw[0]),
          (inwsM.value = n),
          INPUTKEYTEXT.savehist(n);
      },
      speEngine: function (e, t, a) {
        var n = getIat("searpoint", "li"),
          s = n.length;
        if (se_cu == e)
          (ce_wn = t),
            (sea_e = a),
            (UE_NERO.ue.w[se_cu].n = ce_wn),
            (UE_NERO.ue.SE_N[se_cu][ce_wn] = sea_e),
            CORE.switchFication(ce_wn);
        else {
          (se_cu = e),
            (ce_wn = t),
            (sea_e = a),
            (UE_NERO.ue.n = se_cu),
            (UE_NERO.ue.w[se_cu].n = ce_wn),
            (UE_NERO.ue.SE_N[se_cu][ce_wn] = sea_e);
          for (let e = 0; e < s; e++)
            e == se_cu
              ? (n[e].className = "active")
              : n[e].removeAttribute("class");
          USERCONF.write();
        }
      },
    },
    NAVIGATE = {
      writeNavmenu: function () {
        var e = UE_NERO.ue.v.vw,
          t = UE_NERO.na,
          a = t.length;
        let n = UE_NERO.ue.v.vm,
          s = UE_NERO.ue.v.vs[n];
        var i = UE_NERO.ne,
          o = i.length;
        let l = UE_NERO.ue.v.ve,
          r = l.length;
        n >= a && (n = 0);
        for (let e = 0; e < a; e++) {
          var c = document.createElement("li");
          (c.className = "baccllh bacc_b"),
            c.setAttribute("data-index", e),
            (c.innerText = t[e].c),
            e == n &&
              (0 == s
                ? (c.className = "baccllh bacc_b all")
                : (c.className = "baccllh bacc_b half")),
            navlevel.appendChild(c);
        }
        (!l || l.length < 1) &&
          ((l = ["生活", "品牌", "服务"]),
          (r = l.length),
          (UE_NERO.ue.v.ve = l));
        for (let t = 0; t < o; t++) {
          var u = document.createElement("li");
          (u.className = "bacc_b"), (u.innerText = i[t]);
          for (let e = 0; e < r; e++)
            l[e] == i[t] && (u.className = "bacc_b active");
          navlabel.appendChild(u);
        }
        addEvent(navlevel, "click", function (e) {
          e = (e.target || e.srcElement).getAttribute("data-index");
          null != e &&
            ((e = ~~e), (s = UE_NERO.ue.v.vs[e]), NAVIGATE.setNavlevel(e, ~~s));
        }),
          addEvent(navlabel, "click", function (e) {
            e = (e.target || e.srcElement).innerText;
            2 == e.length && NAVIGATE.setNavlabe(e);
          }),
          1 == e
            ? ((navselect.className = "navselect labelmar"),
              (navdetail.className = "navdetail labelmar"),
              (RMC_N[1].a = !1),
              NAVIGATE.setNavlabe())
            : NAVIGATE.setNavlevel(n, s);
      },
      setNavlevel: function (t, a) {
        "number" != typeof t &&
          "number" != typeof a &&
          ((t = UE_NERO.ue.v.vm), (a = UE_NERO.ue.v.vs[t]));
        let n = [];
        var s = [],
          e = UE_NERO.na,
          i = e.length;
        i - 1 < t ? (t = 0) : t < 0 && (t = i - 1);
        let o = e[t].s;
        var l = o.length;
        l < a ? (a = 0) : a < 0 && (a = l);
        let r = getIat("navlevel", "li");
        var c = r.length;
        let u = SO_NERO.length;
        for (let e = 0; e < c; e++)
          e == t && 0 == a
            ? (r[e].className = "baccllh bacc_b all")
            : e == t
            ? (r[e].className = "baccllh bacc_b half")
            : (r[e].className = "baccllh bacc_b");
        if (
          ((navlsm.className = "navlsm hide ftf-tra"),
          (navlsm.innerHTML = ""),
          o[0].sc &&
            (((i = document.createElement("li")).className = "forch"),
            (i.innerText = "全部"),
            navlsm.appendChild(i),
            addEvent(i, "click", function (e) {
              (r[t].className = "bacc_b all"), NAVIGATE.setNavlevsm(n, t, 0);
            })),
          0 != a)
        ) {
          var d = o[a - 1].su,
            m = d.length;
          for (let t = 0; t < m; t++)
            for (let e = 0; e < u; e++)
              if (SO_NERO[e].u.slice(0, 4) == d[t]) {
                s.push(SO_NERO[e]);
                break;
              }
        }
        for (let e = 0; e < l; e++) {
          var _,
            E,
            f = o[e].su,
            p = f.length;
          for (let t = 0; t < p; t++)
            for (let e = 0; e < u; e++)
              if (SO_NERO[e].u.slice(0, 4) == f[t]) {
                n.push(SO_NERO[e]);
                break;
              }
          o[0].sc &&
            ((_ = o[e].sc),
            ((E = document.createElement("li")).className = "forch"),
            E.setAttribute("data-index", e + 1),
            (E.innerHTML = _),
            navlsm.appendChild(E),
            (navlsm.className = "navlsm ftf-tra"),
            addEvent(E, "click", function (e) {
              var e = (e.target || e.srcElement).getAttribute("data-index"),
                a = [],
                n = o[e - 1].su,
                s = n.length,
                e = ~~e;
              r[t].className = "bacc_b half";
              for (let t = 0; t < s; t++)
                for (let e = 0; e < u; e++)
                  if (SO_NERO[e].u.slice(0, 4) == n[t]) {
                    a.push(SO_NERO[e]);
                    break;
                  }
              NAVIGATE.setNavlevsm(a, t, e);
            }));
        }
        0 == a ? this.setNavlevsm(n, t, a) : this.setNavlevsm(s, t, a);
      },
      setNavlevsm: function (e, t, a) {
        var n = getIat("navlsm", "li"),
          s = n.length;
        (UE_NERO.ue.v.vm = ~~t), (UE_NERO.ue.v.vs[t] = ~~a);
        for (let e = 0; e < s; e++)
          e == a
            ? (n[e].className = "forch forc bacc_b active")
            : (n[e].className = "forch");
        this.writeNavsite(e), USERCONF.saveUSERUE();
      },
      setNavlabe: function (t) {
        let n;
        var s = [],
          e = SO_NERO.length,
          i = UE_NERO.ue.v.ve;
        let o = i.length;
        var a = getIat("navlabel", "li"),
          l = a.length;
        if (t)
          for (let e = 0; e < o + 1; e++) {
            if (e == o) {
              4 < (o = i.unshift(t)) && (i.pop(), (o = i.length));
              break;
            }
            if (t == i[e]) {
              1 < o && i.splice(e, 1);
              break;
            }
          }
        for (let t = 0; t < l; t++)
          for (let e = 0; e < o; e++) {
            if (a[t].innerText == i[e]) {
              a[t].className = "bacc_b active";
              break;
            }
            a[t].className = "bacc_b";
          }
        for (let a = 0; a < e; a++) {
          n = 0;
          var r = SO_NERO[a].a.split("/");
          for (let t = 0; t < r.length; t++)
            for (let e = 0; e < o; e++)
              r[t] == i[e] &&
                (n
                  ? 1 == n &&
                    ((SO_NERO[a].m = !0), s.unshift(SO_NERO[a]), s.pop())
                  : ((SO_NERO[a].m = !1), s.push(SO_NERO[a])),
                n++);
        }
        (UE_NERO.ue.v.ve = i),
          NAVIGATE.writeNavsite(s, !0),
          USERCONF.saveUSERUE();
      },
      setNavfind: function (e) {
        let t, a;
        for (
          var n = SO_NERO.length, s = [], i = new RegExp(e), o = 0;
          o < n + 1;
          o++
        ) {
          if (o == n) return 0 < s.length && (this.writeNavsite(s), s.length);
          (t = SO_NERO[o].m || ""),
            (a = (a =
              SO_NERO[o].t +
              t +
              SO_NERO[o].c +
              SO_NERO[o].a +
              SO_NERO[o].i).replace(
              /\/|\:|\.|https|http|www|com|cn/gi,
              ""
            )).match(i) && s.push(SO_NERO[o]);
        }
      },
      writeNavsite: function (n, s) {
        var i = UE_NERO.ue.c.act,
          e = n.length;
        let t;
        var o = parseInt(320 / (e < 64 ? e : 64)),
          l = document.createElement("div");
        let a = getId("navsite-" + del_lastnav_n);
        a &&
          ((a.className += " hide"),
          UE_NERO.ue.c.act
            ? (t = setTimeout(function () {
                remEle(a), clearTimeout(t);
              }, 800))
            : remEle(a)),
          del_lastnav_n++,
          (foc_lastnav_n = 0),
          (foc_lastnav_l = !1),
          (l.id = "navsite-" + del_lastnav_n),
          (l.className = "navsite");
        for (let a = 0; a < e; a++) {
          var r = document.createElement("div"),
            c = document.createElement("img"),
            u = document.createElement("p"),
            d = document.createElement("span");
          let e = n[a].t;
          var m = n[a].i,
            _ = n[a].u.slice(0, 4);
          let t = "images/logo/";
          DARK ? (t += "dark/") : (t += "light/"),
            (c.src = t + _ + ".png" + url_ver),
            (c.onload = function () {
              this.style.opacity = "1";
            }),
            (c.onerror = function () {
              (this.src = t + "logo.png" + url_ver), (this.onerror = null);
            }),
            -1 != e.indexOf("/") && (e = n[a].t.split("/")[0]),
            (c.alt = e),
            (u.innerText = e),
            n[a].m && s
              ? (r.className = "brands meets")
              : (r.className = "brands"),
            r.setAttribute("data-index", m),
            2 == i &&
              ((m =
                (m =
                  (m = "-webkit-animation-delay: " + (_ = o * a) + "ms; ") +
                  "-moz-animation-delay: " +
                  _ +
                  "ms; -ms-animation-delay: " +
                  _ +
                  "ms; ") +
                "-o-animation-delay: " +
                _ +
                "ms; animation-delay: " +
                _ +
                "ms;"),
              r.setAttribute("style", m)),
            r.appendChild(c),
            r.appendChild(u),
            n[a].c && ((d.innerText = n[a].c), r.appendChild(d)),
            l.appendChild(r);
        }
        navdetail.appendChild(l),
          addEvent(l, "click", function (e) {
            e = (e.target || e.srcElement).getAttribute("data-index");
            e &&
              (window.open(e, openw[1]),
              USERCONF.upAtion("NOM", e.replace(/\?|\&/g, "_"), openw[1]));
          });
      },
      focNavsite: function (e, t) {
        var a = getId("navsite-" + del_lastnav_n),
          n = getIat("navsite-" + del_lastnav_n, "div"),
          s = n.length,
          i = a.clientHeight,
          o = a.clientWidth,
          l = a.scrollLeft,
          i = parseInt(i / 80);
        e && 0 == foc_lastnav_n && t
          ? (foc_lastnav_n = 1)
          : "up" == e
          ? --foc_lastnav_n < 1 && (foc_lastnav_n = s)
          : "down" == e
          ? ++foc_lastnav_n > s && (foc_lastnav_n = 1)
          : "left" == e
          ? (foc_lastnav_n -= i) < 1 &&
            (foc_lastnav_n += s - (s % i) + i) > s &&
            (foc_lastnav_n -= i)
          : "right" == e &&
            (foc_lastnav_n += i) > s &&
            (foc_lastnav_n = foc_lastnav_n % i || i);
        for (var r = 0; r < s; r++)
          r == foc_lastnav_n - 1
            ? (addClass(n[r], "hover"),
              (foc_lastnav_l = n[r].getAttribute("data-index")))
            : remClass(n[r], "hover");
        e &&
          (o - 72 < (t = 260 * Math.ceil(foc_lastnav_n / i)) - l &&
            horScroll(a, t - o + 72),
          t - l < 332) &&
          horScroll(a, t - 260 - 72);
      },
    };
  function checkScheme(e, t) {
    let a = !1;
    var n;
    (a =
      1 == e ||
      (2 == e &&
        ((e =
          window.matchMedia &&
          window.matchMedia("(prefers-color-scheme: dark)").matches),
        (n = 0 < navigator.userAgent.indexOf("dark_mode")),
        e || n))
        ? !0
        : a) != DARK &&
      (a
        ? INTERFACE.facialclassName("ski-dark")
        : INTERFACE.facialclassName("ski-light"),
      (DARK = a),
      INTERFACE.cutTheme(now_co, UE_NERO.ue.c.col),
      INSTALLSET.setPictr("0"),
      INFORMATION.getInfo("bg", t),
      INTERFACE.setUp(63, 2400));
  }
  addEvent(sort_cue, "click", function (e) {
    (hasClass(this, "close")
      ? (INTERFACE.facialclassName("bri-sho"),
        (UE_NERO.ue.c.cue = !0),
        remClass)
      : (INTERFACE.facialclassName("bri-hid"),
        (UE_NERO.ue.c.cue = !1),
        addClass))(this, "close"),
      USERCONF.saveUSERUE();
  }),
    addEvent(sort_den, "click", function (e) {
      INTERFACE.cancelDefault(e), (sort_def.className = "show");
    }),
    addEvent(sort_def, "click", function (e) {
      var t = e.target || e.srcElement,
        a = t.getAttribute("data-index");
      if ((INTERFACE.cancelDefault(e), null != a)) {
        var n = getIat(sort_def, "li"),
          s = n.length;
        for (let e = 0; e < s; e++) n[e].className = "baccllh";
        (t.className = "baccllh active"),
          (UE_NERO.ue.o = a.toString()),
          (sort_den.innerText = t.innerText),
          NOW_SO.u.split("-")[0] == UE_NERO.ue.o
            ? (RMC_E[1].a = !1)
            : (RMC_E[1].a = !0),
          (sort_def.className = "hide"),
          USERCONF.saveUSERUE();
      }
    }),
    addEvent(sort_his, "click", function (e) {
      (hasClass(this, "close")
        ? ((UE_NERO.ue.c.his = !0), remClass)
        : ((inwsmcan.style.display = "none"),
          (keylists_se = 0),
          (UE_NERO.ue.c.his = !1),
          addClass))(this, "close"),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_hip, "click", function (e) {
      (hasClass(this, "close")
        ? (entryfield.setAttribute("style", "background: none;"),
          (inaniu.style.display = "none"),
          (implement.style.display = "none"),
          (UE_NERO.ue.c.hip = !0),
          remClass)
        : (entryfield.removeAttribute("style"),
          (inaniu.style.display = "block"),
          (implement.style.display = "block"),
          (UE_NERO.ue.c.hip = !1),
          addClass))(this, "close"),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_pre, "click", function (e) {
      (hasClass(this, "close")
        ? ((UE_NERO.ue.c.pre = !0), remClass)
        : ((UE_NERO.ue.c.pre = !1), addClass))(this, "close"),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_tit, "focus", function (e) {
      sort_tit_blur = !1;
    }),
    addEvent(sort_tit, "keyup", function (e) {
      13 == e.keyCode && sort_tit.blur(), (document.title = sort_tit.value);
    }),
    addEvent(sort_tit, "blur", function (e) {
      let t = sort_tit.value.replace(reg_spc, "$1");
      "" == t && (t = "一个开始"),
        (UE_NERO.ue.c.tit = t),
        USERCONF.saveUSERUE(),
        (document.title = t),
        (sort_tit.value = t),
        (sort_tit_blur = !0);
    }),
    addEvent(sort_skil, "click", function (e) {
      checkScheme(0), (UE_NERO.ue.c.ski = 0), USERCONF.saveUSERUE();
    }),
    addEvent(sort_skid, "click", function (e) {
      checkScheme(1), (UE_NERO.ue.c.ski = 1), USERCONF.saveUSERUE();
    }),
    addEvent(sort_skia, "click", function (e) {
      checkScheme(2), (UE_NERO.ue.c.ski = 2), USERCONF.saveUSERUE();
    }),
    addEvent(sort_col, "click", function (e) {
      hasClass(this, "close")
        ? (INSTALLSET.locaTheme(1, !0), addClass(shuttle, "bacc"))
        : (INSTALLSET.locaTheme(0, !0),
          remClass(shuttle, "bacc"),
          (now_co = ce_co[ce_wn]),
          INTERFACE.cutTheme(now_co),
          addClass(this, "close"));
    }),
    addEvent(huepick, "click", function (e) {
      e = [e.offsetX, e.offsetY];
      INTERFACE.setHSL(e);
    }),
    addEvent(huepick, "mousedown", function (e) {
      0 == e.button && (huepick_mousedown = !0);
    }),
    addEvent(huepick, "mousemove", function (e) {
      huepick_mousedown && ((e = [e.offsetX, e.offsetY]), INTERFACE.setHSL(e));
    }),
    addEvent(huepick, "mouseup", function (e) {
      huepick_mousedown = !1;
    }),
    addEvent(huepick, "mouseleave", function (e) {
      huepick_mousedown = !1;
    }),
    addEvent(huepoint, "click", function (e) {
      INTERFACE.cancelDefault(e), sort_icv.blur();
      e = co_mix;
      e != UE_NERO.ue.c.cot[0] &&
        (11 < UE_NERO.ue.c.cot.unshift(e) && UE_NERO.ue.c.cot.pop(),
        INSTALLSET.setTheme("1", !1));
    }),
    addEvent(huepoint, "mousedown", function (e) {
      INTERFACE.cancelDefault(e);
    }),
    addEvent(huepoint, "mousemove", function (e) {
      INTERFACE.cancelDefault(e);
    }),
    addEvent(satpick, "click", function (e) {
      e = e.offsetX - 34;
      INTERFACE.setHSL(!1, e);
    }),
    addEvent(satpick, "mousedown", function (e) {
      0 == e.button && (satpick_mousedown = !0);
    }),
    addEvent(satpick, "mousemove", function (e) {
      satpick_mousedown && ((e = e.offsetX - 34), INTERFACE.setHSL(!1, e));
    }),
    addEvent(satpick, "mouseup", function (e) {
      satpick_mousedown = !1;
    }),
    addEvent(satpick, "mouseleave", function (e) {
      satpick_mousedown = !1;
    }),
    addEvent(ligpick, "click", function (e) {
      e = e.offsetX - 34;
      INTERFACE.setHSL(!1, !1, e);
    }),
    addEvent(ligpick, "mousedown", function (e) {
      0 == e.button && (ligpick_mousedown = !0);
    }),
    addEvent(ligpick, "mousemove", function (e) {
      ligpick_mousedown && ((e = e.offsetX - 34), INTERFACE.setHSL(!1, !1, e));
    }),
    addEvent(ligpick, "mouseup", function (e) {
      ligpick_mousedown = !1;
    }),
    addEvent(ligpick, "mouseleave", function (e) {
      ligpick_mousedown = !1;
    }),
    addEvent(sort_icv, "focus", function (e) {
      sort_icv_blur = !1;
    }),
    addEvent(sort_icv, "keyup", function (e) {
      13 == e.keyCode && sort_icv.blur();
    }),
    addEvent(sort_icv, "blur", function (e) {
      var t,
        a = sort_icv.value.replace(reg_spc, "$1").replace(reg_isp, "");
      let n = a.toHex();
      n
        ? ((n = n.replace(/#/, "").toLowerCase()),
          (co_mix = co_mix.replace(/#/, "").toLowerCase()),
          n != co_mix &&
            ((t = n
              .toHsl()
              .replace(/(?:\(|\)|hsl|a|%|\s+)*/g, "")
              .split(",")),
            INTERFACE.setHSL(t[0], 2 * t[1], 2 * t[2], n)))
        : "" != a
        ? INTERFACE.pusMess("呃...需要确定的色值~", "normal", 4)
        : (sort_icv.value = co_mix.toUpperCase()),
        (sort_icv_blur = !0);
    }),
    addEvent(sort_bac, "click", function (e) {
      (hasClass(this, "close")
        ? (INSTALLSET.locaPictr(1, !0), remClass)
        : (INSTALLSET.locaPictr(0, !0),
          INFORMATION.getInfo("bg"),
          addClass))(this, "close");
    }),
    addEvent(sort_tra, "change", function (e) {
      var t = ~~this.value;
      (filtermask.style.opacity = 1 - t / 100),
        (UE_NERO.ue.c.tra = t),
        USERCONF.saveUSERUE(),
        INTERFACE.setUp(64),
        INTERFACE.setUp(65, 3600);
    }),
    addEvent(sort_tra, "dblclick", function (e) {
      (filtermask.style.opacity = "0.8"), (UE_NERO.ue.c.tra = 20);
      let t;
      (t = setTimeout(function () {
        (sort_tra.value = UE_NERO.ue.c.tra), clearTimeout(t);
      }, 100)),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_fons, "click", function (e) {
      INTERFACE.facialclassName("ftw-slim"),
        (UE_NERO.ue.c.fon = 0),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_fonn, "click", function (e) {
      INTERFACE.facialclassName("ftw-norm"),
        (UE_NERO.ue.c.fon = 1),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_fonw, "click", function (e) {
      INTERFACE.facialclassName("ftw-wide"),
        (UE_NERO.ue.c.fon = 2),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_actj, "click", function (e) {
      INTERFACE.facialclassName("act-just"),
        (UE_NERO.ue.c.act = 0),
        (rolling_factor = [128, 16, 1.5]),
        USERCONF.saveUSERUE(),
        INTERFACE.setUp(62, 800);
    }),
    addEvent(sort_actf, "click", function (e) {
      INTERFACE.facialclassName("act-fast"),
        (UE_NERO.ue.c.act = 1),
        (rolling_factor = [64, 4, 1.25]),
        USERCONF.saveUSERUE(),
        INTERFACE.setUp(62, 1e3);
    }),
    addEvent(sort_acts, "click", function (e) {
      INTERFACE.facialclassName("act-slow"),
        (UE_NERO.ue.c.act = 2),
        (rolling_factor = [16, 2, 1.05]),
        USERCONF.saveUSERUE(),
        INTERFACE.setUp(62, 1200);
    }),
    addEvent(sort_leaa, "click", function (e) {
      (UE_NERO.ue.c.lea = 0), USERCONF.saveUSERUE();
    }),
    addEvent(sort_leab, "click", function (e) {
      (UE_NERO.ue.c.lea = 1), USERCONF.saveUSERUE();
    }),
    addEvent(sort_leac, "click", function (e) {
      (UE_NERO.ue.c.lea = 2), USERCONF.saveUSERUE();
    }),
    addEvent(sort_swi, "click", function (e) {
      (hasClass(this, "close")
        ? ((UE_NERO.ue.c.swi = !0), remClass)
        : ((UE_NERO.ue.c.swi = !1), addClass))(this, "close"),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_opea, "click", function (e) {
      UE_NERO.ue.c.ope[0]
        ? ((UE_NERO.ue.c.ope[0] = 0), (openw[0] = "_blank"))
        : ((UE_NERO.ue.c.ope[0] = 1), (openw[0] = "_self")),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_opeb, "click", function (e) {
      UE_NERO.ue.c.ope[1]
        ? ((UE_NERO.ue.c.ope[1] = 0), (openw[1] = "_blank"))
        : ((UE_NERO.ue.c.ope[1] = 1), (openw[1] = "_self")),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_opec, "click", function (e) {
      UE_NERO.ue.c.ope[2]
        ? ((UE_NERO.ue.c.ope[2] = 0), (openw[2] = "_blank"))
        : ((UE_NERO.ue.c.ope[2] = 1), (openw[2] = "_self")),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_cop, "click", function (e) {
      (hasClass(this, "close")
        ? ((UE_NERO.ue.c.cop = !0),
          USERCONF.write(),
          sort_copli.setAttribute("style", "height: 256px;"),
          remClass)
        : ((UE_NERO.ue.c.cop = !1),
          USERCONF.write(),
          sort_copli.removeAttribute("style"),
          addClass))(this, "close"),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_loc, "click", function (e) {
      (hasClass(this, "close")
        ? ((UE_NERO.ue.c.loc = !0), remClass)
        : ((UE_NERO.ue.c.loc = !1), addClass))(this, "close"),
        USERCONF.saveUSERUE();
    }),
    addEvent(sort_export, "click", function (e) {
      INTERFACE.pusMess("不好意思，这个功能还没开放！", "fail", 6);
    }),
    addEvent(sort_reset, "click", function (e) {
      INTERFACE.setUp(56);
    }),
    addEvent(sent_joi, "click", function (e) {
      INTERFACE.setUp(31);
    }),
    addEvent(sent_con, "click", function (e) {
      INTERFACE.setUp(32);
    });
  const INSTALLSET = {
      utilize: function (e, t) {
        let a;
        if (
          (this.setCurl(e),
          UE_NERO.ue.c.cue ||
            (INTERFACE.facialclassName("bri-hid"), addClass(sort_cue, "close")),
          UE_NERO.ue.o)
        ) {
          var n = getIat(sort_def, "li"),
            s = n.length;
          for (let e = 0; e < s; e++)
            n[e].getAttribute("data-index") == UE_NERO.ue.o
              ? ((n[e].className = "baccllh active"),
                (sort_den.innerText = n[e].innerText))
              : (n[e].className = "baccllh");
        }
        UE_NERO.ue.c.pre && remClass(sort_pre, "close"),
          UE_NERO.ue.c.his || addClass(sort_his, "close"),
          sort_tit.setAttribute("spellcheck", "false"),
          sort_tit.setAttribute("autocapitalize", "off"),
          sort_tit.setAttribute("autocomplete", "off"),
          sort_tit.setAttribute("autocorrect", "off"),
          sort_tit.setAttribute("maxlength", "32"),
          "一个开始" != UE_NERO.ue.c.tit &&
            ((sort_tit.value = UE_NERO.ue.c.tit),
            (document.title = UE_NERO.ue.c.tit)),
          1 == UE_NERO.ue.c.ski || is_invisible
            ? (INTERFACE.facialclassName("ski-dark"),
              (sort_skid.checked = !0),
              (DARK = !0))
            : 2 == UE_NERO.ue.c.ski &&
              (checkScheme(2, t), (sort_skia.checked = !0));
        var i = 100,
          o = Math.sqrt(2) * i,
          l = Math.PI / 180,
          r = 2 * Math.PI,
          c =
            ((huewheel.width = huewheel.height = 200),
            huewheel.getContext("2d")),
          u = 0.5 * l + 0.02;
        c.translate(i, i),
          c.rotate(-Math.PI / 2),
          c.translate(-i, -i),
          c.arc(i, i, 101, 0, r),
          c.clip();
        for (let e = 0; e < 360; e += 0.5) {
          var d = e * l,
            m = Math.min(r, d + u),
            _ = c.createRadialGradient(i, i, 86, i, i, 88);
          _.addColorStop(0, "hsla(" + e + ", 100%, 50%, 0)"),
            _.addColorStop(1, "hsla(" + e + ", 100%, 50%, 1)"),
            c.beginPath(),
            (c.fillStyle = _),
            c.moveTo(i, i),
            c.arc(i, i, o, d, m),
            c.closePath(),
            c.fill();
        }
        sort_icv.setAttribute("spellcheck", "false"),
          sort_icv.setAttribute("autocapitalize", "off"),
          sort_icv.setAttribute("autocomplete", "off"),
          sort_icv.setAttribute("autocorrect", "off"),
          sort_icv.setAttribute("maxlength", "24"),
          co_mix &&
            ((e = co_mix
              .toHsl()
              .replace(/(?:\(|\)|hsl|a|%|\s+)*/g, "")
              .split(",")),
            INTERFACE.setHSL(e[0], 2 * e[1], 2 * e[2], co_mix)),
          20 != UE_NERO.ue.c.tra &&
            ((sort_tra.value = UE_NERO.ue.c.tra),
            (filtermask.style.opacity = 1 - sort_tra.value / 100)),
          UE_NERO.ue.c.hip &&
            (entryfield.setAttribute("style", "background: none;"),
            (inaniu.style.display = "none"),
            (implement.style.display = "none"),
            remClass(sort_hip, "close")),
          0 == UE_NERO.ue.c.fon
            ? (INTERFACE.facialclassName("ftw-slim"), (sort_fons.checked = !0))
            : 2 == UE_NERO.ue.c.fon &&
              (INTERFACE.facialclassName("ftw-wide"), (sort_fonw.checked = !0)),
          1 == UE_NERO.ue.c.lea
            ? (sort_leab.checked = !0)
            : 2 == UE_NERO.ue.c.lea && (sort_leac.checked = !0),
          UE_NERO.ue.c.swi && remClass(sort_swi, "close"),
          UE_NERO.ue.c.ope[0] &&
            ((sort_opea.checked = !0), (openw[0] = "_self")),
          UE_NERO.ue.c.ope[1] &&
            ((sort_opeb.checked = !0), (openw[1] = "_self")),
          UE_NERO.ue.c.ope[2] &&
            ((sort_opec.checked = !0), (openw[2] = "_self")),
          UE_NERO.ue.c.cop &&
            (sort_copli.setAttribute("style", "height: 256px;"),
            remClass(sort_cop, "close")),
          inwsM.setAttribute("spellcheck", "false"),
          inwsM.setAttribute("autocapitalize", "off"),
          inwsM.setAttribute("autocomplete", "off"),
          inwsM.setAttribute("autocorrect", "off"),
          inwsM.setAttribute("maxlength", "128"),
          inwsT.setAttribute("spellcheck", "false"),
          inwsT.setAttribute("autocapitalize", "off"),
          inwsT.setAttribute("autocomplete", "off"),
          inwsT.setAttribute("autocorrect", "off"),
          inwsT.setAttribute("maxlength", "256"),
          nav_find.setAttribute("spellcheck", "false"),
          nav_find.setAttribute("autocapitalize", "off"),
          nav_find.setAttribute("autocomplete", "off"),
          nav_find.setAttribute("autocorrect", "off"),
          nav_find.setAttribute("maxlength", "8"),
          this.setTheme(UE_NERO.ue.c.col, !0),
          this.setPictr(UE_NERO.ue.c.bac),
          this.storageUsed(),
          this.getUVTotal(),
          NAVIGATE.writeNavmenu(),
          TODOITEM.initTodo(),
          INFORMATION.getInfo("all", t),
          (overload.className = "overload"),
          (overmask.className = "overmask rity"),
          (a = setTimeout(function () {
            (overload.style.display = "none"),
              clearInterval(overload_tg),
              clearTimeout(a);
          }, 800)),
          (hide_langtr_dy = setTimeout(function () {
            0 == UE_NERO.ue.c.act
              ? (INTERFACE.facialclassName("act-just"),
                (rolling_factor = [128, 16, 1.5]),
                (sort_actj.checked = !0))
              : 1 == UE_NERO.ue.c.act
              ? (INTERFACE.facialclassName("act-fast"),
                (rolling_factor = [64, 4, 1.25]),
                (sort_actf.checked = !0))
              : 2 == UE_NERO.ue.c.act &&
                (INTERFACE.facialclassName("act-slow"),
                (rolling_factor = [16, 2, 1.05]),
                (sort_acts.checked = !0)),
              (overmask.className = "overmask"),
              (lang_trch.className = "lang_trch hide"),
              (searpoint.className = "searpoint"),
              clearTimeout(hide_langtr_dy);
          }, 2e3));
      },
      setCurl: function (e) {
        var t,
          a,
          n,
          s,
          i = localStorage.getItem("AcceptS");
        i
          ? ((t = (i = JSON.parse(i)).u),
            (a = i.c),
            (n = i.n),
            (s = i.m),
            (i = i.night),
            reg_num.test(t) &&
              ((UE_NERO.ue.x = 0), (t = AUTO.engLoca(t))) &&
              CORE.speEngine(t[0], t[1], t[2]),
            a && ((UE_NERO.ue.x = 0), AUTO.writeKeyword(a, !0)),
            n &&
              (INTERFACE.setUp(6),
              (UE_NERO.ue.v.vw = 0),
              (t = ~~n.substr(0, 2)),
              (a = ~~n.substr(2, 2)),
              (UE_NERO.ue.v.vm = t),
              (UE_NERO.ue.v.vs[t] = a),
              NAVIGATE.setNavlevel(t, a)),
            s && INTERFACE.pusMess(decodeURIComponent(s), "normal", 12),
            UE_NERO.ue.x || inwsM.focus(),
            i && (is_invisible = !0),
            localStorage.removeItem("AcceptS"))
          : 1 == UE_NERO.ue.x
          ? INTERFACE.setUp(6)
          : (inwsM.focus(), "first" != e || is_invisible || AUTO.firstMeet());
      },
      setTheme: function (e, t) {
        var a = UE_NERO.ue.c.cot;
        let n = a.length;
        var s = document.createElement("li"),
          i = document.createElement("input"),
          o = document.createElement("label");
        sort_cot.innerHTML = "";
        for (let e = 0; e < n; e++) {
          var l,
            r,
            c = a[e].toHex();
          c
            ? ((l = document.createElement("li")),
              (r = document.createElement("span")),
              l.setAttribute("data-index", e),
              r.setAttribute("style", "background-color: " + c + ";"),
              l.appendChild(r),
              sort_cot.appendChild(l),
              addEvent(l, "click", function (e) {
                e = (e.target || e.srcElement).getAttribute("data-index");
                INSTALLSET.locaTheme(1 + ~~e, !0);
              }))
            : (a.splice(e, 1), (UE_NERO.ue.c.cot = a), e--, n--);
        }
        ie11 ||
          ((s.id = "sort_cotinli"),
          (i.type = "color"),
          (i.value = co_mix),
          (i.id = "sort_cotin"),
          (o.innerText = "颜色"),
          o.setAttribute("for", "sort_cotin"),
          s.appendChild(i),
          s.appendChild(o),
          sort_cot.appendChild(s)),
          e
            ? (addClass(shuttle, "bacc"), INSTALLSET.locaTheme(~~e, t))
            : (addClass(sort_col, "close"), INTERFACE.cutTheme(now_co)),
          addEvent(i, "click", function (e) {
            INTERFACE.cancelDefault(e),
              (hasClass(sort_colli, "open") ? remClass : addClass)(
                sort_colli,
                "open"
              );
          });
      },
      locaTheme: function (t, a) {
        var n,
          s = getIat("sort_cot", "li"),
          i = s.length;
        i - 1 < t && (t = 1);
        for (let e = 0; e < i; e++)
          e == t - 1
            ? ((now_co = UE_NERO.ue.c.cot[e]),
              (s[e].className = "active"),
              INTERFACE.cutTheme(now_co, !0),
              remClass(sort_col, "close"),
              a &&
                ((n = (co_mix = now_co)
                  .toHsl()
                  .replace(/(?:\(|\)|hsl|a|%|\s+)*/g, "")
                  .split(",")),
                INTERFACE.setHSL(n[0], 2 * n[1], 2 * n[2], co_mix)))
            : s[e].removeAttribute("class");
        (UE_NERO.ue.c.col = t), USERCONF.saveUSERUE();
      },
      setPictr: function (e) {
        var a = UE_NERO.ue.c.bat;
        let n = a.length,
          o = document.createElement("li"),
          l = document.createElement("input");
        var t = document.createElement("label");
        sort_bat.innerHTML = "";
        for (let t = 0; t < n; t++) {
          var s = document.createElement("li"),
            i = document.createElement("img"),
            r = document.createElement("i");
          if (
            (s.setAttribute("data-index", t),
            (r.className = "icon delete"),
            r.setAttribute("data-index", t),
            "set" == a[t].substr(0, 3))
          ) {
            let e;
            (e = "images/album/"),
              DARK ? (e += "dark/") : (e += "light/"),
              (i.src = e + a[t] + ".jpg" + url_ver);
          } else {
            if (!localStorage.getItem(a[t])) {
              UE_NERO.ue.c.bat.splice(t, 1), t--, n--;
              continue;
            }
            (i.src = localStorage.getItem(a[t])), s.appendChild(r);
          }
          (i.onload = function () {
            this.style.opacity = "1";
          }),
            (i.alt = "自定义壁纸" + (t + 1)),
            s.appendChild(i),
            sort_bat.appendChild(s),
            addEvent(s, "click", function (e) {
              e = (e.target || e.srcElement).getAttribute("data-index");
              INSTALLSET.locaPictr(1 + ~~e, !0);
            }),
            addEvent(r, "click", function (e) {
              INTERFACE.cancelDefault(e);
              e = (e.target || e.srcElement).getAttribute("data-index");
              INSTALLSET.delPictr(~~e);
            });
        }
        (o.id = "sort_batli"),
          (l.type = "file"),
          (l.accept = "image/jpeg"),
          (l.id = "sort_batin"),
          (t.innerText = "图片"),
          t.setAttribute("for", "sort_batin"),
          o.appendChild(l),
          o.appendChild(t),
          sort_bat.appendChild(o),
          0 < e
            ? (remClass(sort_bac, "close"), INSTALLSET.locaPictr(~~e))
            : (addClass(sort_bac, "close"),
              (UE_NERO.ue.c.bac = 0),
              USERCONF.saveUSERUE()),
          addEvent(l, "change", function (e) {
            var t,
              a,
              n,
              s,
              i = this.files[0];
            /^image\//.test(i.type)
              ? 2097152 < i.size
                ? (INTERFACE.pusMess("大了大了，求小于 2MB 的图 ~", "fail", 6),
                  (l.value = ""))
                : ((t = document.createElement("img")),
                  (a = new FileReader()),
                  ((n = document.createElement("div")).className = "partload"),
                  (o.className = "not"),
                  o.appendChild(n),
                  INTERFACE.setUp(64),
                  (t.file = i),
                  (a.onload =
                    ((s = t),
                    function (e) {
                      (s.src = e.target.result), INSTALLSET.savePictr(s.src);
                    })),
                  a.readAsDataURL(i))
              : (INTERFACE.pusMess("格式不支持，找个 jpg 的图呗 ~", "fail", 6),
                (l.value = ""));
          });
      },
      locaPictr: function (t, a) {
        var n = getIat("sort_bat", "li"),
          s = n.length;
        let i = document.createElement("div");
        s <= t ? (t = 1) : t < 0 && (t = 0), (i.className = "partload");
        for (let e = 0; e < s; e++)
          if (e == t - 1) {
            var o = UE_NERO.ue.c.bat[e];
            let t = document.createElement("img");
            if (
              (a && INTERFACE.setUp(64),
              (t.className = "motifi"),
              n[e].appendChild(i),
              "set" == o.substr(0, 3))
            ) {
              let e = "images/album/";
              var l = "set.full." + o.substr(4, 2) + ".jpg" + url_ver;
              DARK ? (e += "dark/") : (e += "light/"), (t.src = e + l);
            } else t.src = localStorage.getItem(o);
            (t.alt = "aurone"),
              innermotif.insertBefore(t, filtermask),
              addEvent(t, "load", function () {
                let e, n;
                i.parentNode.removeChild(i),
                  (e = setTimeout(function () {
                    (t.className = "motifi show"), clearTimeout(e);
                  }, 100)),
                  a && INTERFACE.setUp(65, 3600),
                  (n = setTimeout(function () {
                    var t = getClass(innermotif, "img", "motifi"),
                      a = t.length;
                    if (1 < a)
                      for (let e = 0; e < a - 1; e++)
                        innermotif.removeChild(t[e]);
                    clearTimeout(n);
                  }, 1600));
              }),
              (n[e].className = "active"),
              remClass(sort_bac, "close");
          } else n[e].removeAttribute("class");
        (UE_NERO.ue.c.bac = t), USERCONF.saveUSERUE();
      },
      savePictr: function (e) {
        let t;
        let a = UE_NERO.ue.c.bat.length;
        try {
          localStorage.setItem("Aurpict", e),
            (t = setTimeout(function () {
              localStorage.getItem("Aurpict") &&
                (6 == a && UE_NERO.ue.c.bat.unshift("Aurpict"),
                INSTALLSET.setPictr("1"),
                INTERFACE.setUp(65, 3600),
                INSTALLSET.storageUsed()),
                clearTimeout(t);
            }, 3e3));
        } catch (e) {
          INTERFACE.pusMess(
            "貌似储存空间满了，删掉自定义图片再试一次 ~",
            "fail",
            6
          );
        }
      },
      delPictr: function (e) {
        let t,
          a = UE_NERO.ue.c.bat[e];
        var n = getIat("sort_bat", "li");
        try {
          var s = document.createElement("div");
          localStorage.removeItem(a),
            (s.className = "partload"),
            (n[e].className = "not"),
            n[e].appendChild(s),
            (t = setTimeout(function () {
              localStorage.getItem(a) ||
                (UE_NERO.ue.c.bat.shift(),
                INSTALLSET.setPictr(UE_NERO.ue.c.bac - 1),
                INSTALLSET.storageUsed()),
                clearTimeout(t);
            }, 4e3));
        } catch (e) {
          INTERFACE.pusMess("竟然没删掉！刷新下页面再试一次！", "fail", 6);
        }
      },
      storageUsed: function () {
        var e = (JSON.stringify(localStorage).length / 1024 / 1024).toFixed(2);
        (getId("capacity_used").innerText = "已用 " + e + " M"),
          (getId("capacity_free").innerText =
            "空余 " + (4 - e).toFixed(2) + " M"),
          (getId("sort_capacity_progress").style.width = 25 * e + "%");
      },
      getUVTotal: function () {
        let t = new XMLHttpRequest();
        (t.onreadystatechange = function () {
          var e;
          4 == t.readyState &&
            200 == t.status &&
            ((e = t.responseText.replace(/(\d)(?=(\d{3})+$)/g, "$1,")),
            (sort_total.innerText = e));
        }),
          t.open("GET", INITURL.v, !0),
          t.setRequestHeader("If-Modified-Since", "0"),
          t.send();
      },
    },
    TODOITEM =
      (addEvent(worknew, "click", function (e) {
        e = ~~(e.target || e.srcElement).getAttribute("data-index");
        0 == e
          ? "worknew" == worknew.className
            ? (worknew.className = "worknew open")
            : (worknew.className = "worknew")
          : 9 == e
          ? INTERFACE.setUp(12)
          : TODOITEM.newTodolist(e);
      }),
      {
        initTodo: function () {
          var e = localStorage.getItem("Aurtodo");
          UE_NERO.ue.t &&
            (INTERFACE.setUp(11), e) &&
            TODOITEM.readTodolist(!1, e);
        },
        newTodolist: function (e) {
          let t;
          var a = ITEM_ON.length;
          if (
            (64 < a &&
              ((e = 0),
              INTERFACE.pusMess("不好意思，只能创建这么多了！", "fail", 6)),
            1 == e)
          )
            t = { u: 1, o: !0, c: !1 };
          else if (2 == e) t = { u: 2, o: !0, t: !1, c: !1 };
          else if (3 == e) t = { u: 3, o: !0, c: !1 };
          else if (4 == e) t = { u: 4, o: !0, t: !1, c: !1 };
          else if (5 == e) {
            for (let e = 0; e < a; e++)
              if (5 == ITEM_ON[e].u) {
                ITEM_ON.splice(e, 1);
                break;
              }
            t = { u: 5, o: !0, c: !1 };
          } else
            6 == e
              ? (t = { u: 6, o: !0, t: !1, c: !1 })
              : 7 == e && (t = { u: 7, o: !0, t: !1, c: !1 });
          e &&
            (ITEM_ON.unshift(t),
            TODOITEM.readTodolist(!0),
            (worknew.className = "worknew"));
        },
        readTodolist: function (e, t) {
          var a;
          (workitem.innerHTML = ""),
            (a = (ITEM_ON = !e && t ? JSON.parse(t) : ITEM_ON).length),
            (worktotal.innerText = a ? a + " 个事项" : "没有事项");
          for (let e = 0; e < a; e++) {
            var n = ITEM_ON[e],
              s = document.createElement("div"),
              i = document.createElement("div");
            switch (
              ((s.className = "blank"),
              (i.className = "partload"),
              (s.id = "ntitem" + e),
              s.appendChild(i),
              workitem.appendChild(s),
              n.u)
            ) {
              case 1:
                TODOITEM.new_todo(n, e);
                break;
              case 2:
                TODOITEM.new_note(n, e);
                break;
              case 3:
                TODOITEM.new_book(n, e);
                break;
              case 4:
                TODOITEM.new_alar(n, e);
                break;
              case 5:
                TODOITEM.new_hots(n, e);
                break;
              case 6:
                TODOITEM.new_news(n, e);
                break;
              case 7:
                TODOITEM.new_radi(n, e);
            }
          }
          for (let e = 0; e < a; e++) {
            if (ITEM_ON[e].o) {
              RMC_W[2].a = !0;
              break;
            }
            RMC_W[2].a = !1;
          }
          for (let e = 0; e < a; e++) {
            if (!ITEM_ON[e].o) {
              RMC_W[3].a = !0;
              break;
            }
            RMC_W[3].a = !1;
          }
          for (let e = 0; e < a; e++) {
            if (5 == ITEM_ON[e].u || 6 == ITEM_ON[e].u) {
              RMC_W[1].a = !0;
              break;
            }
            RMC_W[1].a = !1;
          }
          0 == a && ((RMC_W[1].a = !1), (RMC_W[2].a = !1), (RMC_W[3].a = !1)),
            TODOITEM.saveTodolist();
        },
        new_todo: function (e, t) {
          INTERFACE.pusMess("非常抱歉，暂时无法使用 TODO 功能", "fail", 6);
        },
        new_note: function (e, a) {
          let n = getId("ntitem" + a);
          var t = document.createElement("p");
          let s = document.createElement("input"),
            i = document.createElement("textarea");
          var o = ["置顶", "收起", "删除"];
          let l = document.createElement("ul");
          l.className = "noteset";
          for (let t = 0; t < 3; t++) {
            let e;
            var r = document.createElement("li");
            0 == a && 0 == t
              ? ((e = 4), (o[t] = "清空"))
              : 0 == t
              ? (e = 1)
              : 1 == t
              ? (e = 2)
              : 2 == t && (e = 3),
              (r.innerText = o[t]),
              r.setAttribute("data-index", a + "-" + e),
              l.appendChild(r),
              addEvent(r, "click", function (e) {
                e = (e.target || e.srcElement)
                  .getAttribute("data-index")
                  .split("-");
                TODOITEM.trolTodolist(~~e[0], ~~e[1]);
              });
          }
          e.o
            ? ((n.className = "worknote"), (t.innerText = "便笺"))
            : ((n.className = "worknote close"),
              "" != e.t ? (t.innerText = e.t) : (t.innerText = "便笺")),
            (t.className = "title forch"),
            s.setAttribute("type", "text"),
            s.setAttribute("maxlength", "12"),
            s.setAttribute("placeholder", "标题"),
            s.setAttribute("spellcheck", "false"),
            s.setAttribute("autocapitalize", "off"),
            s.setAttribute("autocomplete", "off"),
            s.setAttribute("autocorrect", "off"),
            i.setAttribute("placeholder", "内容"),
            i.setAttribute("maxlength", "8192"),
            (n.innerHTML = ""),
            n.appendChild(t),
            n.appendChild(s),
            n.appendChild(i),
            "" != e.t && (s.value = e.t),
            "" != e.c && (i.value = e.c),
            (i.style.height = i.scrollHeight + "px"),
            (n.style.height = i.scrollHeight + 89 + "px"),
            n.appendChild(l),
            addEvent(t, "mouseenter", function (e) {
              l.className = "noteset show";
            }),
            addEvent(n, "mouseleave", function (e) {
              l.className = "noteset";
            }),
            addEvent(t, "click", function (e) {
              TODOITEM.trolTodolist(a, 5);
            }),
            addEvent(s, "keyup", function (e) {
              13 == e.keyCode && (s.blur(), i.focus());
              e = s.value.replace(reg_spc, "$1").replace(reg_isp, " ");
              (ITEM_ON[a].t = e), TODOITEM.saveTodolist();
            }),
            addEvent(i, "keyup", function (e) {
              var t = i.value.replace(reg_spc, "$1").replace(reg_isp, " ");
              (ITEM_ON[a].c = t),
                (i.style.height = i.scrollHeight + "px"),
                (n.style.height = i.scrollHeight + 89 + "px"),
                TODOITEM.saveTodolist();
            });
        },
        new_book: function (e, a) {
          var t = getId("ntitem" + a),
            n = document.createElement("p");
          let s = document.createElement("input"),
            i = document.createElement("input");
          var o = ["置顶", "收起", "删除"];
          let l = document.createElement("ul");
          l.className = "noteset";
          for (let t = 0; t < 3; t++) {
            let e;
            var r = document.createElement("li");
            0 == a && 0 == t
              ? ((e = 4), (o[t] = "清空"))
              : 0 == t
              ? (e = 1)
              : 1 == t
              ? (e = 2)
              : 2 == t && (e = 3),
              (r.innerText = o[t]),
              r.setAttribute("data-index", a + "-" + e),
              l.appendChild(r),
              addEvent(r, "click", function (e) {
                e = (e.target || e.srcElement)
                  .getAttribute("data-index")
                  .split("-");
                TODOITEM.trolTodolist(~~e[0], ~~e[1]);
              });
          }
          e.o ? (t.className = "worknote") : (t.className = "worknote close"),
            (n.className = "title forch"),
            (n.innerText = "书签"),
            s.setAttribute("type", "text"),
            s.setAttribute("maxlength", "12"),
            s.setAttribute("placeholder", "标题"),
            s.setAttribute("spellcheck", "false"),
            s.setAttribute("autocapitalize", "off"),
            s.setAttribute("autocomplete", "off"),
            s.setAttribute("autocorrect", "off"),
            i.setAttribute("type", "text"),
            i.setAttribute("maxlength", "256"),
            i.setAttribute("placeholder", "链接"),
            i.setAttribute("spellcheck", "false"),
            i.setAttribute("autocapitalize", "off"),
            i.setAttribute("autocomplete", "off"),
            i.setAttribute("autocorrect", "off"),
            (t.innerHTML = ""),
            t.appendChild(n),
            t.appendChild(s),
            t.appendChild(i),
            "" != e.t && (s.value = e.t),
            "" != e.c && (i.value = e.c),
            t.appendChild(l),
            addEvent(n, "mouseenter", function (e) {
              l.className = "noteset show";
            }),
            addEvent(t, "mouseleave", function (e) {
              l.className = "noteset";
            }),
            addEvent(n, "click", function (e) {
              TODOITEM.trolTodolist(a, 5);
            }),
            addEvent(s, "keyup", function (e) {
              var t = s.value.replace(reg_spc, "$1").replace(reg_isp, " ");
              (ITEM_ON[a].t = t), TODOITEM.saveTodolist();
            }),
            addEvent(i, "keyup", function (e) {
              var t = i.value.replace(reg_spc, "$1").replace(reg_isp, " ");
              (ITEM_ON[a].c = t), TODOITEM.saveTodolist();
            });
        },
        new_alar: function (e, t) {
          INTERFACE.pusMess("非常抱歉，暂时无法使用闹钟功能", "fail", 6);
        },
        new_hots: function (e, a) {
          if (e.c) TODOITEM.write_hots(e, a);
          else {
            e = "c=1&v=" + now_ver + loa_sta;
            let t = new XMLHttpRequest();
            (t.onreadystatechange = function () {
              var e;
              4 == t.readyState &&
                200 == t.status &&
                ((e = JSON.parse(t.responseText)),
                (ITEM_ON[a].c = e),
                TODOITEM.write_hots(ITEM_ON[a], a));
            }),
              t.open("POST", INITURL.n, !0),
              t.setRequestHeader(
                "Content-type",
                "application/x-www-form-urlencoded"
              ),
              t.send(e);
          }
        },
        write_hots: function (t, a) {
          var n,
            s,
            e = getId("ntitem" + a),
            i = document.createElement("p"),
            o = document.createElement("ul"),
            l = ["置顶", "收起", "删除"];
          let r = document.createElement("ul");
          r.className = "noteset";
          for (let t = 0; t < 3; t++) {
            let e;
            var c = document.createElement("li");
            0 == a && 0 == t
              ? ((e = 0), (l[t] = "刷新"))
              : 0 == t
              ? (e = 1)
              : 1 == t
              ? (e = 2)
              : 2 == t && (e = 3),
              (c.innerText = l[t]),
              c.setAttribute("data-index", a + "-" + e),
              r.appendChild(c),
              addEvent(c, "click", function (e) {
                e = (e.target || e.srcElement)
                  .getAttribute("data-index")
                  .split("-");
                TODOITEM.trolTodolist(~~e[0], ~~e[1]);
              });
          }
          t.o ? (e.className = "workhots") : (e.className = "workhots close"),
            (i.className = "title forch"),
            (i.innerText = "百度热榜"),
            (o.className = "hots"),
            (n = (t.c[0].clicks - t.c[8].clicks) / 100),
            (s = t.c[8].clicks);
          for (let e = 0; e < 9; e++) {
            var u = document.createElement("li"),
              d = ~~t.c[e].clicks,
              m = d / 1e4,
              d =
                '<b><i style="width: ' +
                ((d - s) / n).toFixed(2) +
                '%;"></i></b>';
            (u.innerHTML =
              "<span>" + m.toFixed(2) + " 万</span>" + d + t.c[e].title),
              o.appendChild(u),
              addEvent(u, "click", function (e) {
                e = (e.target || e.srcElement).innerHTML
                  .split("</b>")[1]
                  .split("<span>")[0];
                window.open(b_searc[0] + encodeURI(e), openw[2]),
                  USERCONF.upAtion("HOT");
              });
          }
          (e.innerHTML = ""),
            e.appendChild(i),
            e.appendChild(o),
            e.appendChild(r),
            addEvent(i, "mouseenter", function (e) {
              r.className = "noteset show";
            }),
            addEvent(e, "mouseleave", function (e) {
              r.className = "noteset";
            }),
            addEvent(i, "click", function (e) {
              TODOITEM.trolTodolist(a, 5);
            });
        },
        new_news: function (e, n) {
          let s = getId("ntitem" + n);
          var t = document.createElement("p");
          let i = document.createElement("input"),
            o = document.createElement("div");
          var a = ["置顶", "收起", "删除"];
          let l = document.createElement("ul");
          l.className = "noteset";
          for (let t = 0; t < 3; t++) {
            let e;
            var r = document.createElement("li");
            0 == n && 0 == t
              ? ((e = 0), (a[t] = "刷新"))
              : 0 == t
              ? (e = 1)
              : 1 == t
              ? (e = 2)
              : 2 == t && (e = 3),
              (r.innerText = a[t]),
              r.setAttribute("data-index", n + "-" + e),
              l.appendChild(r),
              addEvent(r, "click", function (e) {
                e = (e.target || e.srcElement)
                  .getAttribute("data-index")
                  .split("-");
                TODOITEM.trolTodolist(~~e[0], ~~e[1]);
              });
          }
          e.o
            ? (s.className = "worknews airy")
            : (s.className = "worknews close"),
            (t.className = "title forch"),
            (t.innerText = "实时资讯"),
            (o.className = "sumy"),
            (o.innerText = "请输入一个关键词，例如：华为、房价、成都，等。"),
            i.setAttribute("type", "text"),
            i.setAttribute("maxlength", "12"),
            i.setAttribute("placeholder", "请输入关键词"),
            i.setAttribute("spellcheck", "false"),
            i.setAttribute("autocapitalize", "off"),
            i.setAttribute("autocomplete", "off"),
            i.setAttribute("autocorrect", "off"),
            "" != e.t &&
              ((i.value = e.t), TODOITEM.tain_news(encodeURI(e.t), n)),
            (s.innerHTML = ""),
            s.appendChild(t),
            s.appendChild(i),
            s.appendChild(o),
            s.appendChild(l),
            addEvent(t, "mouseenter", function (e) {
              l.className = "noteset show";
            }),
            addEvent(s, "mouseleave", function (e) {
              l.className = "noteset";
            }),
            addEvent(t, "click", function (e) {
              TODOITEM.trolTodolist(n, 5);
            }),
            addEvent(i, "keyup", function (e) {
              13 == e.keyCode && i.blur();
            }),
            addEvent(i, "blur", function (e) {
              var t,
                a = i.value.replace(reg_spc, "$1").replace(reg_isp, " ");
              "" != a &&
                ((t = getClass(s, "ul", "news")),
                (s.className = "worknews airy"),
                (o.innerHTML = ""),
                o.setAttribute("class", "partload"),
                o.setAttribute("style", " display: block; top: 70%;"),
                t && remEle(t),
                (ITEM_ON[n].t = a),
                TODOITEM.tain_news(encodeURI(ITEM_ON[n].t), n),
                TODOITEM.saveTodolist());
            });
        },
        tain_news: function (e, t) {
          e = "c=" + e + "&v=" + now_ver + loa_sta;
          let a = new XMLHttpRequest();
          (a.onreadystatechange = function () {
            var e;
            4 == a.readyState &&
              200 == a.status &&
              ((e = a.responseText), TODOITEM.read_news(e, t));
          }),
            a.open("POST", INITURL.w, !0),
            a.setRequestHeader(
              "Content-type",
              "application/x-www-form-urlencoded"
            ),
            a.send(e);
        },
        read_news: function (e, t) {
          if (128 <= e.length) {
            var a = [],
              n = e.split("<br>"),
              s = /<a href="|<\/span>/gi,
              i = /" target="_blank">|<\/a>&nbsp;<span>|<\/a> <span>/gi;
            for (let e = 0; e < 9; e++) {
              var o = { l: !1, t: !1, s: !1 },
                l = n[e].replace(s, "").split(i);
              (o.l = l[0].replace(/&#x/g, "%u").replace(/;/g, "")),
                (o.t = l[1]),
                (o.s = l[2]),
                a.push(o);
            }
            (ITEM_ON[t].c = a), TODOITEM.write_news(ITEM_ON[t], t);
          } else {
            e = getIat("ntitem" + t, "div");
            e.setAttribute("class", "sumy"),
              e.setAttribute("style", "display: block;"),
              (e.innerText =
                "很抱歉，暂时没有相关信息，请输入其他关键词或稍后再试。");
          }
        },
        write_news: function (t, e) {
          var a = getId("ntitem" + e),
            n = (getClass(a, "ul", "noteset"), getIat(a, "div")),
            s = document.createElement("ul");
          s.className = "news";
          for (let e = 0; e < 9; e++) {
            var i = document.createElement("li"),
              o = document.createElement("span");
            i.setAttribute("data-index", unescape(t.c[e].l)),
              (i.innerHTML = t.c[e].t),
              (o.innerHTML = t.c[e].s),
              i.appendChild(o),
              s.appendChild(i),
              t.o && (a.className = "worknews"),
              n.setAttribute("style", "display: none;"),
              a.appendChild(s),
              addEvent(i, "click", function (e) {
                e = (e.target || e.srcElement).getAttribute("data-index");
                window.open(e, openw[2]), USERCONF.upAtion("NEW");
              });
          }
        },
        new_radi: function (e, t) {
          INTERFACE.pusMess("非常抱歉，暂时无法使用电台功能", "fail", 6);
        },
        trolTodolist: function (e, t) {
          var a = getId("ntitem" + e),
            n = getIat(a, "p"),
            s = ITEM_ON.length;
          if (0 == t) (ITEM_ON[e].c = !1), TODOITEM.readTodolist(!0);
          else if (1 == t) {
            var i = ITEM_ON[e];
            ITEM_ON.splice(e, 1), ITEM_ON.unshift(i), TODOITEM.readTodolist(!0);
          } else if (2 == t) {
            2 == ITEM_ON[e].u &&
              "" != ITEM_ON[e].t &&
              (n.innerText = ITEM_ON[e].t),
              (a.className += " close"),
              (ITEM_ON[e].o = !1),
              (RMC_W[3].a = !0);
            for (let e = 0; e < s; e++) {
              if (ITEM_ON[e].o) {
                RMC_W[2].a = !0;
                break;
              }
              RMC_W[2].a = !1;
            }
            TODOITEM.saveTodolist();
          } else if (3 == t)
            (2 != ITEM_ON[e].u ||
              ("" == ITEM_ON[e].t && "" == ITEM_ON[e].c) ||
              confirm("删除后无法恢复，确定继续？")) &&
              (ITEM_ON.splice(e, 1), TODOITEM.readTodolist(!0));
          else if (4 == t)
            2 != ITEM_ON[e].u ||
              ("" == ITEM_ON[e].t && "" == ITEM_ON[e].c) ||
              (confirm("清空后无法恢复，确定继续？") &&
                ((ITEM_ON[e].t = !1),
                (ITEM_ON[e].c = !1),
                TODOITEM.readTodolist(!0)));
          else if (5 == t) {
            i = a.className;
            2 == ITEM_ON[e].u && (n.innerText = "便笺"),
              (a.className = i.replace(" close", "")),
              (ITEM_ON[e].o = !0),
              (RMC_W[2].a = !0);
            for (let e = 0; e < s; e++) {
              if (!ITEM_ON[e].o) {
                RMC_W[3].a = !0;
                break;
              }
              RMC_W[3].a = !1;
            }
            TODOITEM.saveTodolist();
          }
        },
        saveTodolist: function () {
          var t = ITEM_ON,
            a = t.length;
          for (let e = 0; e < a; e++)
            5 == t[e].u && (t[e].c = !1), 6 == t[e].u && (t[e].c = !1);
          localStorage.setItem("Aurtodo", JSON.stringify(t));
        },
      }),
    INFORMATION = {
      getInfo: function (t, e) {
        let a, n;
        !single_url && e && (UP++, (single_url = INITURL[e.charAt(UP++)])),
          ((n = new XMLHttpRequest()).onreadystatechange = function () {
            var e;
            4 == n.readyState &&
              200 == n.status &&
              ((e = n.getResponseHeader("Date")),
              (e = new Date(e)),
              (a = JSON.parse(n.responseText)),
              INFORMATION.writeinfo(a, e, t));
          }),
          n.open("GET", single_url, !0),
          n.send();
      },
      writeinfo: function (e, a, t) {
        var n = localStorage.getItem("Aurmeet"),
          n = JSON.parse(n) || [];
        if ("all" == t) {
          let t = document.createElement("span");
          if (
            (this.singleday(e.sgle, a),
            IMPORTANCE.getServicetime(a.getTime(), e.fest),
            UE_NERO.ue.c.bac || this.writeBackground(e.wall, a),
            e.news && this.realnews(e.news, a),
            n[4] == guidecourse.getAttribute("data-ver") || is_invisible)
          ) {
            if (e.intro)
              for (var s = e.intro.length, i = 0; i < s; i++) {
                var o = e.intro[i];
                INFORMATION.introduce(o, a);
              }
            (t.className = "quickstart"),
              (t.innerHTML = "<i>\\</i>快速上手"),
              copyright.appendChild(t),
              addEvent(t, "click", function (e) {
                remEle(t), INFORMATION.introduction("ative");
              });
          } else this.introduction("new");
        } else if ("face" == t) {
          if (n[4] == guidecourse.getAttribute("data-ver") && e.intro)
            for (var l = e.intro.length, i = 0; i < l; i++) {
              var r = e.intro[i];
              INFORMATION.introduce(r, a);
            }
          this.singleday(e.sgle, a),
            IMPORTANCE.getServicetime(a.getTime(), e.fest),
            this.writeBackground(e.wall, a);
        } else "bg" == t && this.writeBackground(e.wall, a);
        this.svcWorker(), this.followUp(a, 20);
      },
      writeBackground: function (n, s) {
        var i;
        let e = 1 + ~~s.getMonth(),
          t = s.getDate();
        var o = n.length;
        let l = document.createElement("img");
        e < 10 && (e = "0" + e.toString()),
          t < 10 && (t = "0" + t.toString()),
          (i = e.toString() + t.toString()),
          (l.className = "motifi");
        for (let a = 0; a < o; a++) {
          var r = new Date(),
            c = new Date(),
            u = n[a].d.split("-"),
            d = u[0].split("/"),
            m = ~~d[0],
            _ = ~~d[1] - 1,
            d = ~~d[2],
            u = u[1].split("/"),
            E = ~~u[0],
            f = ~~u[1] - 1,
            u = ~~u[2];
          let p = n[a].t
            ? ((e = n[a].t.split("-")), (t = e[0].split(":")), e[1].split(":"))
            : ((t = ["00", "01"]), ["23", "59"]);
          var g = ~~t[0],
            v = ~~t[1],
            E = ~~p[0],
            f = ~~p[1];
          if (
            (r.setFullYear(m, _, d),
            r.setHours(g, v, 0),
            c.setFullYear(E, f, u),
            c.setHours(E, f, u),
            r <= s && s < c)
          ) {
            let e = "images/news/";
            DARK ? (e += "dark/") : (e += "light/"),
              (l.src = e + n[a].i + url_ver);
          } else if (a == o - 1) {
            let e = "images/album/";
            DARK ? (e += "dark/") : (e += "light/"),
              (l.src = e + i + ".jpg" + url_ver);
          }
          (l.alt = "今日壁纸"),
            (l.onerror = function () {
              let e = "images/background-";
              DARK ? (e += "dark.jpg") : (e += "light.jpg"),
                (l.src = e + url_ver),
                (l.onerror = null);
            });
        }
        innermotif.insertBefore(l, filtermask),
          addEvent(l, "load", function () {
            let e, n;
            (e = setTimeout(function () {
              (l.className += " show"), clearTimeout(e);
            }, 100)),
              (n = setTimeout(function () {
                var t = getClass(innermotif, "img", "motifi"),
                  a = t.length;
                if (1 < a)
                  for (let e = 0; e < a - 1; e++) innermotif.removeChild(t[e]);
                clearTimeout(n);
              }, 1600));
          });
      },
      singleday: function (t, e) {
        var a, n, s, i;
        if (e) {
          (n = e.getFullYear()),
            (s = 1 + ~~e.getMonth()),
            (e = e.getDate()),
            (i = t.length),
            (a = n + "/" + s + "/" + e);
          for (let e = 0; e < i; e++)
            a == t[e].d && t[e].c && (air_noti.innerText = t[e].c);
          airnotice_time_delay = setTimeout(function () {
            (airnotice.className = "airnotice note ftf-tra"),
              clearTimeout(airnotice_time_delay);
          }, 6e3);
        } else
          clearTimeout(airnotice_time_delay),
            (n = "<em>" + t.split("").join("</em><em>") + "</em>"),
            (air_noti.innerHTML = n),
            (air_noti.className = "air_noti forc"),
            (airnotice.className = "airnotice note ftf-tra"),
            (airnotice_time = !1);
        addEvent(air_noti, "click", function () {
          copyText(air_noti.innerText)
            ? INTERFACE.pusMess("已复制到剪贴板！", "carry", 4)
            : INTERFACE.pusMess("复制失败", "fail", 4);
        });
      },
      realnews: function (s, e) {
        var t = new Date(),
          a = new Date(),
          n = s.t.split("-"),
          i = n[0].split(","),
          o = i[0].split("/"),
          l = ~~o[0],
          r = ~~o[1] - 1,
          o = ~~o[2],
          i = i[1].split(":"),
          c = ~~i[0],
          i = ~~i[1],
          n = n[1].split(","),
          u = n[0].split("/"),
          d = ~~u[0],
          m = ~~u[1] - 1,
          u = ~~u[2],
          n = n[1].split(":"),
          _ = ~~n[0],
          n = ~~n[1];
        if (
          (t.setFullYear(l, r, o),
          t.setHours(c, i, 0),
          a.setFullYear(d, m, u),
          a.setHours(_, n, 0),
          t <= e && e < a)
        ) {
          let t = document.createElement("img"),
            a = document.createElement("div");
          l = document.createElement("span");
          let e = "images/news/";
          (t.id = "newsposter"),
            (t.className = "motifi"),
            DARK ? (e += "dark/") : (e += "light/"),
            (t.src = e + s.i),
            (t.alt = s.h),
            (a.className = "newslogan"),
            (a.innerText = s.h),
            (l.innerText = "关闭海报"),
            a.appendChild(l),
            addClass(air_noti, "hide");
          let n;
          (n = setTimeout(function () {
            innermotif.insertBefore(t, filtermask),
              topengine.insertBefore(a, attention),
              clearTimeout(n);
          }, 6e3)),
            addEvent(t, "load", function () {
              let e;
              e = setTimeout(function () {
                (t.className = "motifi show"),
                  (a.className = "newslogan show"),
                  clearTimeout(e);
              }, 7e3);
            });
        }
      },
      introduce: function (n, e) {
        var t = new Date(),
          a = new Date(),
          s = n.t.split("-"),
          i = s[0].split(","),
          o = i[0].split("/"),
          l = ~~o[0],
          r = ~~o[1] - 1,
          o = ~~o[2],
          i = i[1].split(":"),
          c = ~~i[0],
          i = ~~i[1],
          s = s[1].split(","),
          u = s[0].split("/"),
          d = ~~u[0],
          m = ~~u[1] - 1,
          u = ~~u[2],
          s = s[1].split(":"),
          _ = ~~s[0],
          s = ~~s[1];
        if (
          (t.setFullYear(l, r, o),
          t.setHours(c, i, 0),
          a.setFullYear(d, m, u),
          a.setHours(_, s, 0),
          t <= e && e < a)
        ) {
          let t = document.createElement("b");
          (l = document.createElement("img")),
            (r = document.createElement("p")),
            (o = document.createElement("p")),
            (c = document.createElement("img"));
          let e = "attention show use",
            a = "images/intro/";
          DARK ? (a += "dark/") : (a += "light/"),
            (attention.innerHTML = ""),
            (t.innerText = n.c),
            (t.className = "label"),
            (l.src = a + n.i + url_ver),
            (l.alt = n.h),
            (l.className = "brand"),
            (r.innerText = n.h),
            (r.className = "title"),
            (o.innerText = n.d),
            (o.className = "sub"),
            n.as && attention.setAttribute("style", n.as),
            n.is && l.setAttribute("style", n.is),
            n.hs && r.setAttribute("style", n.hs),
            n.ds && o.setAttribute("style", n.ds),
            n.os && c.setAttribute("style", n.os),
            attention.appendChild(t),
            attention.appendChild(l),
            attention.appendChild(r),
            attention.appendChild(o),
            n.o &&
              ((c.src = a + n.o + url_ver),
              (c.alt = n.h),
              (c.className = "poster"),
              (e = "attention show open"),
              attention.appendChild(c)),
            addEvent(l, "load", function () {
              let e;
              (attention.className = "attention show"),
                (e = setTimeout(function () {
                  (attention.className = "attention show use"), clearTimeout(e);
                }, 2e3));
            }),
            addEvent(attention, "mouseenter", function () {
              (t.innerText = "关闭"),
                (attention.className = e),
                clearTimeout(attention_height_delay);
            }),
            addEvent(attention, "mouseleave", function () {
              (t.innerText = n.c),
                clearTimeout(attention_height_delay),
                (attention_height_delay = setTimeout(function () {
                  (attention.className = "attention show use"),
                    clearTimeout(attention_height_delay);
                }, 800));
            }),
            addEvent(attention, "click", function () {
              window.open(n.l, openw[2]),
                USERCONF.upAtion("SEL", n.l.replace(/\?|\&/g, "_"), openw[2]);
            }),
            addEvent(t, "mouseenter", function () {
              let e;
              (e = DARK ? "#999" : "#8d9cab"), (t.style.backgroundColor = e);
            }),
            addEvent(t, "mouseleave", function () {
              let e;
              (e = DARK ? "#666" : "#ccc"), (t.style.backgroundColor = e);
            }),
            addEvent(t, "click", function () {
              let e;
              INTERFACE.cancelDefault(event),
                (attention.className = "attention show mini"),
                (e = setTimeout(function () {
                  (attention.className = "attention"),
                    clearTimeout(attention_height_delay),
                    clearTimeout(e);
                }, 600));
            });
        }
      },
      introduction: function (e) {
        let t;
        "ative" == e
          ? ((couclose.innerText = "关闭"),
            (guidecourse_show_delay_time = 100),
            (getId("courselist").className = "courselist"),
            (attention.className = "attention"))
          : "new" == e && (couclose.innerText = "稍后提醒"),
          (guidecourse.style.display = "block"),
          (t = setTimeout(function () {
            (guidecourse.className = "guidecourse show"), clearTimeout(t);
          }, guidecourse_show_delay_time));
      },
      svcWorker: function () {
        "serviceWorker" in navigator &&
          navigator.serviceWorker.register("service.js" + url_ver);
      },
      followUp: function (o, e) {
        let l;
        l = setTimeout(function () {
          let e,
            t,
            a,
            n = "images/album/";
          var s = new Date(),
            i = getId("auronelogo");
          s.setDate(o.getDate() + 1),
            (t = 1 + ~~s.getMonth()),
            (a = s.getDate()),
            t < 10 && (t = "0" + t.toString()),
            a < 10 && (a = "0" + a.toString()),
            (e = t.toString() + a.toString()),
            DARK ? (n += "dark/") : (n += "light/"),
            (i.src = n + e + ".jpg" + url_ver),
            clearTimeout(l);
        }, 1e3 * e);
      },
    };
  let write_in_sequence_delay, progress_end_delay;
  const AUTO = {
      getCoor: function (e) {
        var e = e.getBoundingClientRect(),
          t = [];
        return (
          (t[0] = Math.ceil(e.left + e.width / 2)),
          (t[1] = Math.ceil(e.top + e.height / 2)),
          t
        );
      },
      moveCur: function (e, t, a, n) {
        t || a || ((t = screen_size[0] / 2), (a = screen_size[1] / 2 + 40)),
          (t += 76),
          (a += 76),
          (democursor.style.webkitTransform =
            "translate(" + t + "px, " + a + "px)"),
          (democursor.style.mozTransform =
            "translate(" + t + "px, " + a + "px)"),
          (democursor.style.msTransform =
            "translate(" + t + "px, " + a + "px)"),
          (democursor.style.oTransform = "translate(" + t + "px, " + a + "px)"),
          (democursor.style.transform = "translate(" + t + "px, " + a + "px)"),
          (democursor.style.opacity = e),
          n &&
            setTimeout(function () {
              (democursor.style.webkitTransform =
                "translate(" + t + "px, " + a + "px)"),
                (democursor.style.mozTransform =
                  "translate(" + t + "px, " + a + "px)"),
                (democursor.style.msTransform =
                  "translate(" + t + "px, " + a + "px)"),
                (democursor.style.oTransform =
                  "translate(" + t + "px, " + a + "px)"),
                (democursor.style.transform =
                  "translate(" + t + "px, " + a + "px)"),
                (democursor.style.opacity = "1");
            }, n);
      },
      firstMeet: function () {
        let e, t, a, n, s, i, o;
        (guidecourse_show_delay_time = 5e3),
          (e = setTimeout(function () {
            INTERFACE.pusMess("初次见面，欢迎 ~", "talk", 5, !1, "-48"),
              clearTimeout(e);
          }, 2e3)),
          (t = setTimeout(function () {
            INTERFACE.pusMess("二十秒，百分百上手 ~", "talk", 6),
              clearTimeout(t);
          }, 4e3)),
          (a = setTimeout(function () {
            (o = AUTO.getCoor(guidecourse)), AUTO.moveCur(1), clearTimeout(a);
          }, 6e3)),
          (n = setTimeout(function () {
            (democursor.className = "democursor moving"),
              AUTO.moveCur(1, o[0], o[1]),
              clearTimeout(n);
          }, 7e3)),
          (s = setTimeout(function () {
            (guidecourse.className = "guidecourse show meet"), clearTimeout(s);
          }, 8e3)),
          (i = setTimeout(function () {
            AUTO.moveCur(0, o[0], o[1]),
              (democursor.className = "democursor"),
              clearTimeout(i);
          }, 1e4));
      },
      autoStep1: function () {
        let t = AUTO.engLoca("5513"),
          a = AUTO.engLoca("5400"),
          n = AUTO.engLoca("5014"),
          s = AUTO.engLoca("5001"),
          i = ["#96f", "#87f", "#78f", "#69f"];
        DARK && (i = ["#248", "#426", "#624", "#842"]);
        let o,
          l,
          r,
          c,
          u,
          d,
          m,
          _,
          E,
          f,
          p,
          T,
          g,
          v,
          h,
          N,
          w,
          R,
          C,
          O,
          b,
          I,
          y,
          L,
          D,
          H,
          W,
          z,
          A,
          k,
          U,
          M,
          S,
          x,
          F,
          e;
        AUTO.courseplay(38),
          CORE.speEngine(0, 0, 0),
          (inwsM.value = ""),
          (o = setTimeout(function () {
            (democursor.className = "democursor moving"), clearTimeout(o);
          }, 200)),
          (l = setTimeout(function () {
            var e = getIat("searpoint", "li")[t[0]];
            (A = AUTO.getCoor(e)),
              AUTO.moveCur(1, A[0], A[1] - 4),
              clearTimeout(l);
          }, 1e3)),
          (r = setTimeout(function () {
            AUTO.pointClick(A[0], A[1] - 4),
              CORE.speEngine(t[0], 0, 0),
              clearTimeout(r);
          }, 2e3)),
          (c = setTimeout(function () {
            var e = getIat("fication", "li")[t[1]];
            (k = AUTO.getCoor(e)),
              AUTO.moveCur(1, k[0], k[1] + 12),
              clearTimeout(c);
          }, 3e3)),
          (u = setTimeout(function () {
            AUTO.pointClick(k[0], k[1] + 12),
              CORE.speEngine(t[0], t[1], t[2]),
              clearTimeout(u);
          }, 4e3)),
          (d = setTimeout(function () {
            var e = getIat("engines", "ul")[t[1]].getElementsByTagName("li")[
              t[2]
            ];
            (U = AUTO.getCoor(e)), AUTO.moveCur(1, U[0], U[1]), clearTimeout(d);
          }, 5e3)),
          (m = setTimeout(function () {
            INTERFACE.pusMess(
              "音效，免费商用，海量优质",
              i[0],
              6,
              U[0],
              U[1] + 20,
              1
            ),
              clearTimeout(m);
          }, 6e3)),
          (_ = setTimeout(function () {
            var e = getIat("fication", "li")[a[1]];
            (M = AUTO.getCoor(e)),
              AUTO.moveCur(1, M[0], M[1] + 12),
              clearTimeout(_);
          }, 8500)),
          (E = setTimeout(function () {
            AUTO.pointClick(M[0], M[1] + 12),
              CORE.speEngine(a[0], a[1], a[2]),
              clearTimeout(E);
          }, 9500)),
          (f = setTimeout(function () {
            var e = getIat("engines", "ul")[a[1]].getElementsByTagName("li")[
              a[2]
            ];
            (S = AUTO.getCoor(e)), AUTO.moveCur(1, S[0], S[1]), clearTimeout(f);
          }, 10500)),
          (p = setTimeout(function () {
            INTERFACE.pusMess(
              "视频，免费商用，海量优质",
              i[1],
              6,
              S[0],
              S[1] + 20,
              1
            ),
              clearTimeout(p);
          }, 11500)),
          (T = setTimeout(function () {
            var e = getIat("fication", "li")[n[1]];
            (x = AUTO.getCoor(e)),
              AUTO.moveCur(1, x[0], x[1] + 12),
              clearTimeout(T);
          }, 14e3)),
          (g = setTimeout(function () {
            AUTO.pointClick(x[0], x[1] + 12),
              CORE.speEngine(n[0], n[1], n[2]),
              clearTimeout(g);
          }, 15e3)),
          (v = setTimeout(function () {
            var e = getIat("engines", "ul")[n[1]].getElementsByTagName("li")[
              n[2]
            ];
            (x = AUTO.getCoor(e)), AUTO.moveCur(1, x[0], x[1]), clearTimeout(v);
          }, 16e3)),
          (h = setTimeout(function () {
            INTERFACE.pusMess("矢量插画，免费", i[2], 6, x[0], x[1] + 20, 1),
              clearTimeout(h);
          }, 17e3)),
          (N = setTimeout(function () {
            var e = getIat("engines", "ul")[s[1]].getElementsByTagName("li")[
              s[2]
            ];
            (F = AUTO.getCoor(e)), AUTO.moveCur(1, F[0], F[1]), clearTimeout(N);
          }, 19500)),
          (w = setTimeout(function () {
            AUTO.pointClick(F[0], F[1]),
              CORE.speEngine(s[0], s[1], s[2]),
              input_tran != LANG_IN[2].c &&
                ((input_tran = LANG_IN[2].c),
                CORE.translateSwitch(input_tran, !1)),
              clearTimeout(w);
          }, 20500)),
          (R = setTimeout(function () {
            INTERFACE.pusMess(
              "图库，免费商用，海量优质",
              i[3],
              6,
              F[0],
              F[1] + 20,
              1
            ),
              clearTimeout(R);
          }, 21500)),
          (C = setTimeout(function () {
            var e = AUTO.getCoor(inwsM);
            INTERFACE.pusMess("输入关键词", i[2], 6, e[0], e[1], 1),
              clearTimeout(C);
          }, 24500)),
          (O = setTimeout(function () {
            AUTO.writeKeyword("%E5%92%96%E5%95%A1"), clearTimeout(O);
          }, 25500)),
          (b = setTimeout(function () {
            var e = AUTO.getCoor(getId("inwsT"));
            INTERFACE.pusMess("国外资源，自动翻译", i[1], 6, e[0], e[1], 1),
              clearTimeout(b);
          }, 27500)),
          (I = setTimeout(function () {
            (e = AUTO.getCoor(getId("implo"))),
              AUTO.moveCur(1, e[0], e[1] + 8),
              clearTimeout(I);
          }, 29500)),
          (y = setTimeout(function () {
            (implo.className = "hover"),
              clearTimeout(blinking_implo_delay),
              (blinking_implo_delay = setTimeout(function () {
                implo.removeAttribute("class"),
                  clearTimeout(blinking_implo_delay);
              }, 4500)),
              clearTimeout(y);
          }, 3e4)),
          (L = setTimeout(function () {
            INTERFACE.pusMess(
              "点击搜索，即刻得到",
              i[0],
              7,
              e[0] - 26,
              e[1] + 8,
              2
            ),
              clearTimeout(L);
          }, 31e3)),
          (D = setTimeout(function () {
            AUTO.moveCur(0, e[0], e[1] + 8), clearTimeout(D);
          }, 34e3)),
          (H = setTimeout(function () {
            (democursor.className = "democursor"),
              AUTO.moveCur(0, -80, -80),
              clearTimeout(H);
          }, 35e3)),
          (W = setTimeout(function () {
            (guidecourse.className = "guidecourse show"), clearTimeout(W);
          }, 36e3)),
          (z = setTimeout(function () {
            (getId("courselist").className = "courselist step2"),
              clearTimeout(z);
          }, 37e3)),
          addEvent(window, "keyup", function (e) {
            27 == e.keyCode &&
              (clearTimeout(o),
              clearTimeout(o),
              clearTimeout(l),
              clearTimeout(r),
              clearTimeout(c),
              clearTimeout(u),
              clearTimeout(d),
              clearTimeout(m),
              clearTimeout(_),
              clearTimeout(E),
              clearTimeout(f),
              clearTimeout(p),
              clearTimeout(T),
              clearTimeout(g),
              clearTimeout(v),
              clearTimeout(h),
              clearTimeout(N),
              clearTimeout(w),
              clearTimeout(R),
              clearTimeout(C),
              clearTimeout(O),
              clearTimeout(b),
              clearTimeout(I),
              clearTimeout(y),
              clearTimeout(L),
              clearTimeout(D),
              clearTimeout(H),
              clearTimeout(W),
              clearTimeout(z),
              AUTO.courseplay(!1));
          });
      },
      autoStep2: function () {
        var e = AUTO.engLoca("5001");
        let t = AUTO.engLoca("8200"),
          a = ["#96f", "#87f", "#78f", "#69f"];
        DARK && (a = ["#248", "#426", "#624", "#842"]);
        let n,
          s,
          i,
          o,
          l,
          r,
          c,
          u,
          d,
          m,
          _,
          E,
          f,
          p,
          T,
          g,
          v,
          h,
          N,
          w,
          R,
          C,
          O,
          b,
          I,
          y,
          A,
          k,
          U,
          M,
          S,
          x;
        AUTO.courseplay(36),
          CORE.speEngine(e[0], e[1], e[2]),
          (n = setTimeout(function () {
            (democursor.className = "democursor moving"),
              (inwsM.value = "冰与火之歌"),
              clearTimeout(n);
          }, 200)),
          (s = setTimeout(function () {
            (A = AUTO.getCoor(inwsM)),
              AUTO.moveCur(1, A[0], A[1] + 160),
              clearTimeout(s);
          }, 1e3)),
          (i = setTimeout(function () {
            AUTO.pointClick(A[0], A[1] + 160), clearTimeout(i);
          }, 2e3)),
          (o = setTimeout(function () {
            INTERFACE.setUp(21),
              (inwsmcan.style.display = "none"),
              (inwstcan.style.display = "none"),
              AUTO.pointClick(A[0], A[1] + 160),
              clearTimeout(o);
          }, 2600)),
          (l = setTimeout(function () {
            INTERFACE.pusMess("双击空白处，可清空输入", a[0], 7, A[0], A[1], 1),
              clearTimeout(l);
          }, 3500)),
          (r = setTimeout(function () {
            AUTO.moveCur(1, A[0] + 120, A[1] + 200), clearTimeout(r);
          }, 5e3)),
          (c = setTimeout(function () {
            AUTO.pointClick(A[0] + 120, A[1] + 200), clearTimeout(c);
          }, 6e3)),
          (u = setTimeout(function () {
            CORE.speEngine(0, 0, 0),
              AUTO.pointClick(A[0] + 120, A[1] + 200),
              clearTimeout(u);
          }, 6600)),
          (d = setTimeout(function () {
            var e = getIat("engines", "ul")[0].getElementsByTagName("li")[0];
            (k = AUTO.getCoor(e)),
              INTERFACE.pusMess(
                "再次双击，恢复默认搜索",
                a[1],
                7,
                k[0],
                k[1] + 20,
                1
              ),
              clearTimeout(d);
          }, 8e3)),
          (m = setTimeout(function () {
            var e = getIat("fication", "li")[0];
            (x = AUTO.getCoor(e)),
              INTERFACE.pusMess("搜索部分网页时", a[2], 6, x[0], x[1], 4),
              clearTimeout(m);
          }, 12e3)),
          (_ = setTimeout(function () {
            INTERFACE.pusMess("输入知名品牌", a[3], 6, A[0], A[1], 1),
              clearTimeout(_);
          }, 14e3)),
          (E = setTimeout(function () {
            AUTO.writeKeyword("%E6%B7%98%E5%AE%9D"), clearTimeout(E);
          }, 15e3)),
          (f = setTimeout(function () {
            (U = AUTO.getCoor(implo)),
              AUTO.moveCur(1, U[0], U[1]),
              clearTimeout(f);
          }, 17e3)),
          (p = setTimeout(function () {
            (implo.className = "hover"),
              clearTimeout(blinking_implo_delay),
              (blinking_implo_delay = setTimeout(function () {
                implo.removeAttribute("class"),
                  clearTimeout(blinking_implo_delay);
              }, 4500)),
              clearTimeout(p);
          }, 17500)),
          (T = setTimeout(function () {
            INTERFACE.pusMess("可直达官网", a[2], 6, U[0] - 26, U[1] + 8, 2),
              clearTimeout(T);
          }, 18e3)),
          (g = setTimeout(function () {
            var e = getIat("fication", "li")[t[1]];
            (M = AUTO.getCoor(e)),
              AUTO.moveCur(1, M[0], M[1] + 12),
              clearTimeout(g);
          }, 22e3)),
          (v = setTimeout(function () {
            AUTO.pointClick(M[0], M[1] + 12),
              CORE.speEngine(t[0], t[1], 5),
              clearTimeout(v);
          }, 23e3)),
          (h = setTimeout(function () {
            var e = getIat("engines", "ul")[t[1]].getElementsByTagName("li")[
              t[2]
            ];
            (S = AUTO.getCoor(e)), AUTO.moveCur(1, S[0], S[1]), clearTimeout(h);
          }, 24e3)),
          (N = setTimeout(function () {
            AUTO.pointClick(S[0], S[1]),
              CORE.speEngine(t[0], t[1], t[2]),
              clearTimeout(N);
          }, 25e3)),
          (w = setTimeout(function () {
            AUTO.pointClick(S[0], S[1]), clearTimeout(w);
          }, 25600)),
          (R = setTimeout(function () {
            INTERFACE.pusMess("双击名称", a[1], 6, S[0], S[1] + 20, 1),
              clearTimeout(R);
          }, 26500)),
          (C = setTimeout(function () {
            INTERFACE.pusMess("也可直达首页", a[0], 6, S[0], S[1] + 76, 1),
              clearTimeout(C);
          }, 28500)),
          (O = setTimeout(function () {
            AUTO.moveCur(0, S[0], S[1]), clearTimeout(O);
          }, 31500)),
          (b = setTimeout(function () {
            (democursor.className = "democursor"),
              AUTO.moveCur(0, -80, -80),
              clearTimeout(b);
          }, 32e3)),
          (I = setTimeout(function () {
            (guidecourse.className = "guidecourse show"), clearTimeout(I);
          }, 34e3)),
          (y = setTimeout(function () {
            (getId("courselist").className = "courselist step3"),
              clearTimeout(y);
          }, 35e3)),
          addEvent(window, "keyup", function (e) {
            27 == e.keyCode &&
              (clearTimeout(n),
              clearTimeout(n),
              clearTimeout(s),
              clearTimeout(i),
              clearTimeout(o),
              clearTimeout(l),
              clearTimeout(r),
              clearTimeout(c),
              clearTimeout(u),
              clearTimeout(d),
              clearTimeout(m),
              clearTimeout(_),
              clearTimeout(E),
              clearTimeout(f),
              clearTimeout(p),
              clearTimeout(T),
              clearTimeout(g),
              clearTimeout(v),
              clearTimeout(h),
              clearTimeout(N),
              clearTimeout(w),
              clearTimeout(R),
              clearTimeout(C),
              clearTimeout(O),
              clearTimeout(b),
              clearTimeout(I),
              clearTimeout(y),
              AUTO.courseplay(!1));
          });
      },
      autoStep3: function () {
        let e = ["#96f", "#87f", "#78f", "#69f"],
          t = (DARK && (e = ["#248", "#426", "#624", "#842"]), "images/album/");
        DARK ? (t += "dark/") : (t += "light/");
        let a,
          n,
          s,
          i,
          o,
          l,
          r,
          c,
          u,
          d,
          m,
          _,
          E,
          f,
          p,
          T,
          g,
          v,
          h,
          N,
          w,
          R,
          C,
          O,
          b,
          L,
          D,
          H,
          W,
          z,
          P,
          j,
          q,
          K,
          G,
          X,
          Y,
          B,
          $,
          V,
          J,
          Z,
          Q,
          ee,
          te,
          ae,
          ne,
          se,
          ie,
          oe,
          le,
          re,
          ce,
          ue,
          I,
          y,
          de,
          A,
          me,
          k,
          U,
          M,
          S,
          x,
          F;
        AUTO.courseplay(58),
          (a = setTimeout(function () {
            (democursor.className = "democursor moving"), clearTimeout(a);
          }, 200)),
          (n = setTimeout(function () {
            (I = AUTO.getCoor(topcontrnav)),
              AUTO.moveCur(1, I[0] - 8, I[1] + 8),
              clearTimeout(n);
          }, 1e3)),
          (s = setTimeout(function () {
            addClass(topcontrnav, "hover"), clearTimeout(s);
          }, 1600)),
          (i = setTimeout(function () {
            UE_NERO.ue.v.vw && ((UE_NERO.ue.v.vw = 0), INTERFACE.setUp(16)),
              INTERFACE.pusMess("切换至导航", e[0], 6, I[0] - 8, I[1] + 8, 2),
              clearTimeout(i);
          }, 2e3)),
          (o = setTimeout(function () {
            remClass(topcontrnav, "hover"), INTERFACE.setUp(6), clearTimeout(o);
          }, 3e3)),
          (l = setTimeout(function () {
            (y = getIat(navlevel, "li")[1]),
              (de = AUTO.getCoor(y)),
              AUTO.moveCur(1, de[0] + 40, de[1]),
              clearTimeout(l);
          }, 4e3)),
          (r = setTimeout(function () {
            addClass(y, "hover"), clearTimeout(r);
          }, 4500)),
          (c = setTimeout(function () {
            NAVIGATE.setNavlevel(1, 0), clearTimeout(c);
          }, 5e3)),
          (u = setTimeout(function () {
            remClass(y, "hover"),
              (A = getIat(navlevel, "li")[2]),
              (me = AUTO.getCoor(A)),
              AUTO.moveCur(1, me[0] + 40, me[1]),
              clearTimeout(u);
          }, 6e3)),
          (d = setTimeout(function () {
            addClass(A, "hover"), clearTimeout(d);
          }, 6100)),
          (m = setTimeout(function () {
            NAVIGATE.setNavlevel(2, 0), clearTimeout(m);
          }, 7e3)),
          (_ = setTimeout(function () {
            remClass(A, "hover"),
              (k = getIat(navlevel, "li")[3]),
              (U = AUTO.getCoor(k)),
              AUTO.moveCur(1, U[0] + 40, U[1]),
              clearTimeout(_);
          }, 8e3)),
          (E = setTimeout(function () {
            addClass(k, "hover"), clearTimeout(E);
          }, 8100)),
          (f = setTimeout(function () {
            NAVIGATE.setNavlevel(3, 0), clearTimeout(f);
          }, 9e3)),
          (p = setTimeout(function () {
            INTERFACE.pusMess(
              "用心精选、整理和分类",
              e[1],
              6,
              U[0] + 40,
              U[1] + 20,
              1
            ),
              clearTimeout(p);
          }, 1e4)),
          (T = setTimeout(function () {
            remClass(k, "hover"), AUTO.moveCur(1), clearTimeout(T);
          }, 12e3)),
          (g = setTimeout(function () {
            let e,
              t = 16,
              a = getId("navsite-" + del_lastnav_n);
            (e = setInterval(function () {
              (t /= 1.0125),
                (a.scrollLeft += +t.toFixed(2)),
                Math.abs(t) < 2 && clearInterval(e);
            }, 16)),
              clearTimeout(g);
          }, 13e3)),
          (v = setTimeout(function () {
            INTERFACE.pusMess("使用鼠标滚轮滚动", e[2], 6), clearTimeout(v);
          }, 15500)),
          (h = setTimeout(function () {
            (M = AUTO.getCoor(navcontrret)),
              AUTO.moveCur(1, M[0], M[1] + 8),
              clearTimeout(h);
          }, 18e3)),
          (N = setTimeout(function () {
            addClass(navcontrret, "hover"), clearTimeout(N);
          }, 18600)),
          (w = setTimeout(function () {
            INTERFACE.pusMess("返回至搜索", e[3], 6, M[0], M[1] + 8, 2),
              clearTimeout(w);
          }, 19e3)),
          (R = setTimeout(function () {
            remClass(navcontrret, "hover"),
              INTERFACE.setUp(9),
              (auronelogo.src = t + "set.full.01.jpg" + url_ver),
              clearTimeout(R);
          }, 2e4)),
          (C = setTimeout(function () {
            (S = AUTO.getCoor(topcontrset)),
              AUTO.moveCur(1, S[0], S[1] + 8),
              clearTimeout(C);
          }, 21e3)),
          (O = setTimeout(function () {
            addClass(topcontrset, "hover"), clearTimeout(O);
          }, 21200)),
          (b = setTimeout(function () {
            INTERFACE.pusMess("个性化外观", e[2], 6, S[0], S[1] + 64, 2),
              (auronelogo.src = t + "set.full.02.jpg" + url_ver),
              clearTimeout(b);
          }, 22e3)),
          (L = setTimeout(function () {
            remClass(topcontrset, "hover"), INTERFACE.setUp(7), clearTimeout(L);
          }, 23e3)),
          (D = setTimeout(function () {
            AUTO.moveCur(1),
              (auronelogo.src = t + "set.full.03.jpg" + url_ver),
              clearTimeout(D);
          }, 24e3)),
          (H = setTimeout(function () {
            let e = 12,
              t;
            (t = setInterval(function () {
              (e /= 1.0125),
                (innersortlist.scrollTop += +e.toFixed(2)),
                Math.abs(e) < 2 && clearInterval(t);
            }, 16)),
              clearTimeout(H);
          }, 25e3)),
          (W = setTimeout(function () {
            INTERFACE.pusMess("丰富的自定义设置", e[1], 7),
              (auronelogo.src = t + "set.full.04.jpg" + url_ver),
              clearTimeout(W);
          }, 27e3)),
          (z = setTimeout(function () {
            (x = AUTO.getCoor(concontrret)),
              AUTO.moveCur(1, x[0], x[1] + 8),
              clearTimeout(z);
          }, 29e3)),
          (P = setTimeout(function () {
            addClass(concontrret, "hover"), clearTimeout(P);
          }, 29600)),
          (j = setTimeout(function () {
            INTERFACE.pusMess("返回至搜索", e[0], 6, x[0], x[1] + 8, 2),
              clearTimeout(j);
          }, 3e4)),
          (q = setTimeout(function () {
            remClass(concontrret, "hover"), INTERFACE.setUp(9), clearTimeout(q);
          }, 31e3)),
          (K = setTimeout(function () {
            (F = AUTO.getCoor(air_noti)),
              AUTO.moveCur(1, F[0] - 160, F[1] + 16),
              clearTimeout(K);
          }, 32e3)),
          (G = setTimeout(function () {
            remClass(air_time, "ftf-cla"),
              addClass(air_time, "forc"),
              (air_time.innerText = "种一棵树..."),
              clearTimeout(G);
          }, 32500)),
          (X = setTimeout(function () {
            (airnotice.className = "airnotice time ftf-tra"), clearTimeout(X);
          }, 33e3)),
          (Y = setTimeout(function () {
            addClass(air_noti, "forc"),
              (air_noti.innerText = "最好的时间是十年前..."),
              clearTimeout(Y);
          }, 34500)),
          (B = setTimeout(function () {
            (airnotice.className = "airnotice note ftf-tra"), clearTimeout(B);
          }, 35e3)),
          ($ = setTimeout(function () {
            (auronelogo.src = t + "set.full.04.jpg" + url_ver),
              (air_time.innerText = "其次就是现在。"),
              clearTimeout($);
          }, 36500)),
          (V = setTimeout(function () {
            (airnotice.className = "airnotice time ftf-tra"), clearTimeout(V);
          }, 37e3)),
          (J = setTimeout(function () {
            INTERFACE.pusMess(
              "每天都有新句子",
              e[1],
              7,
              F[0] - 160,
              F[1] - 16,
              4
            ),
              clearTimeout(J);
          }, 39e3)),
          (Z = setTimeout(function () {
            INTERFACE.facialclassName("act-fast"),
              (inwsmcan.style.display = "none"),
              (inwstcan.style.display = "none"),
              clearTimeout(Z);
          }, 4e4)),
          (Q = setTimeout(function () {
            (filtermask.style.opacity = "0.6"), clearTimeout(Q);
          }, 41e3)),
          (ee = setTimeout(function () {
            INSTALLSET.locaPictr(2), clearTimeout(ee);
          }, 43e3)),
          (te = setTimeout(function () {
            AUTO.moveCur(1, F[0] - 240, F[1] - 240), clearTimeout(te);
          }, 44e3)),
          (ae = setTimeout(function () {
            INSTALLSET.locaPictr(3), clearTimeout(ae);
          }, 45e3)),
          (ne = setTimeout(function () {
            INSTALLSET.locaPictr(4), clearTimeout(ne);
          }, 47e3)),
          (se = setTimeout(function () {
            INTERFACE.facialclassName("act-slow"), clearTimeout(se);
          }, 48e3)),
          (ie = setTimeout(function () {
            (filtermask.style.opacity = 1 - UE_NERO.ue.c.tra / 100),
              clearTimeout(ie);
          }, 49e3)),
          (oe = setTimeout(function () {
            INTERFACE.pusMess("每天新壁纸", e[2], 6, 40, "-56"),
              clearTimeout(oe);
          }, 5e4)),
          (le = setTimeout(function () {
            AUTO.moveCur(0, F[0] - 240, F[1] - 240),
              (democursor.className = "democursor"),
              INTERFACE.pusMess("桌面主页，从这里开始 ~", e[1], 7, 40),
              clearTimeout(le);
          }, 52e3)),
          (re = setTimeout(function () {
            AUTO.moveCur(0, -80, -80),
              INTERFACE.pusMess("一个开始 ~", e[0], 7, 40, "+56"),
              clearTimeout(re);
          }, 54e3)),
          (ce = setTimeout(function () {
            remClass(air_time, "forc"),
              remClass(air_noti, "forc"),
              addClass(air_time, "ftf-cla"),
              INSTALLSET.locaPictr(0),
              addClass(sort_bac, "close"),
              INFORMATION.getInfo("face"),
              clearTimeout(ce);
          }, 55e3)),
          (ue = setTimeout(function () {
            INTERFACE.setUp(61), clearTimeout(ue);
          }, 57e3)),
          addEvent(window, "keyup", function (e) {
            27 == e.keyCode &&
              (clearTimeout(a),
              clearTimeout(a),
              clearTimeout(n),
              clearTimeout(s),
              clearTimeout(i),
              clearTimeout(o),
              clearTimeout(l),
              clearTimeout(r),
              clearTimeout(c),
              clearTimeout(u),
              clearTimeout(d),
              clearTimeout(m),
              clearTimeout(_),
              clearTimeout(E),
              clearTimeout(f),
              clearTimeout(p),
              clearTimeout(T),
              clearTimeout(g),
              clearTimeout(v),
              clearTimeout(h),
              clearTimeout(N),
              clearTimeout(w),
              clearTimeout(R),
              clearTimeout(C),
              clearTimeout(O),
              clearTimeout(b),
              clearTimeout(L),
              clearTimeout(D),
              clearTimeout(H),
              clearTimeout(W),
              clearTimeout(z),
              clearTimeout(P),
              clearTimeout(j),
              clearTimeout(q),
              clearTimeout(K),
              clearTimeout(G),
              clearTimeout(X),
              clearTimeout(Y),
              clearTimeout(B),
              clearTimeout($),
              clearTimeout(V),
              clearTimeout(J),
              clearTimeout(Z),
              clearTimeout(Q),
              clearTimeout(ee),
              clearTimeout(te),
              clearTimeout(ae),
              clearTimeout(ne),
              clearTimeout(se),
              clearTimeout(ie),
              clearTimeout(oe),
              clearTimeout(le),
              clearTimeout(re),
              clearTimeout(ce),
              clearTimeout(ue),
              AUTO.courseplay(!1));
          });
      },
      courseplay: function (e) {
        let t = UE_NERO.ue.c.hip,
          a = getId("durationline"),
          n;
        t &&
          (entryfield.removeAttribute("style"),
          (inaniu.style.display = "block"),
          (implement.style.display = "block")),
          e
            ? ((overmask.className = "overmask play"),
              (guidecourse.className = "guidecourse hide"),
              a.setAttribute(
                "style",
                cssPrefix("transition-duration", e + "s")
              ),
              2 != UE_NERO.ue.c.act && INTERFACE.facialclassName("act-slow"),
              (progress_end_delay = setTimeout(function () {
                (overmask.className = "overmask rity"),
                  0 == UE_NERO.ue.c.act
                    ? INTERFACE.facialclassName("act-just")
                    : 1 == UE_NERO.ue.c.act &&
                      INTERFACE.facialclassName("act-fast"),
                  t &&
                    (entryfield.setAttribute("style", "background: none;"),
                    (inaniu.style.display = "none"),
                    (implement.style.display = "none")),
                  a.removeAttribute("style"),
                  (n = setTimeout(function () {
                    (overmask.className = "overmask"), clearTimeout(n);
                  }, 200)),
                  clearTimeout(progress_end_delay);
              }, 1e3 * e)))
            : ((overmask.className = "overmask rity"),
              (guidecourse.className = "guidecourse show"),
              0 == UE_NERO.ue.c.act
                ? INTERFACE.facialclassName("act-just")
                : 1 == UE_NERO.ue.c.act &&
                  INTERFACE.facialclassName("act-fast"),
              (democursor.className = "democursor"),
              AUTO.moveCur(0, -80, -80),
              t &&
                (entryfield.setAttribute("style", "background: none;"),
                (inaniu.style.display = "none"),
                (implement.style.display = "none")),
              a.removeAttribute("style"),
              (n = setTimeout(function () {
                (overmask.className = "overmask"), clearTimeout(n);
              }, 200)),
              clearTimeout(progress_end_delay));
      },
      engLoca: function (n) {
        var e = UE_NERO.se.length;
        e: for (let a = 0; a < e; a++) {
          var s = UE_NERO.se[a],
            i = s.length;
          for (let t = 0; t < i; t++) {
            var o = s[t].u,
              l = o.length;
            for (let e = 0; e < l; e++)
              if (n == o[e].split("-")[0]) return [a, t, e];
          }
        }
      },
      writeKeyword: function (e, t) {
        let a = 0,
          n = "";
        let s = decodeURIComponent(e).split(""),
          i = s.length,
          o,
          l;
        clearTimeout(write_in_sequence_delay),
          (inwsM.value = ""),
          (o = function () {
            let e = Math.floor(500 * Math.random());
            t && (e = Math.floor(100 * Math.random())),
              clearTimeout(write_in_sequence_delay),
              (n += s[a]),
              (inwsM.value = n),
              ++a == i
                ? (clearTimeout(write_in_sequence_delay),
                  input_tran && CORE.toTranslate(),
                  CORE.censorKeyword(),
                  INPUTKEYTEXT.getHint())
                : (write_in_sequence_delay = setTimeout(o, e));
          }),
          (l = setTimeout(function () {
            o(), clearTimeout(l);
          }, 400));
      },
      pointClick: function (e, t, a) {
        let n,
          s,
          i = document.createElement("div");
        e &&
          t &&
          ((i.className = "cwater focus"),
          (n = "left: " + ~~e + "px; top: " + ~~t + "px;")),
          (reg_hex.test(a) || reg_rgb.test(a)) &&
            (n += " background-color: " + a + ";"),
          i.setAttribute("style", n),
          innerfeed.appendChild(i),
          (s = setTimeout(function () {
            remEle(i), clearTimeout(s);
          }, 4200));
      },
    },
    SOLARLUNAR = {
      mathlunar: function (e) {
        var n = [
          19416, 19168, 42352, 21717, 53856, 55632, 91476, 22176, 39632, 21970,
          19168, 42422, 42192, 53840, 119381, 46400, 54944, 44450, 38320, 84343,
          18800, 42160, 46261, 27216, 27968, 109396, 11104, 38256, 21234, 18800,
          25958, 54432, 59984, 28309, 23248, 11104, 100067, 37600, 116951,
          51536, 54432, 120998, 46416, 22176, 107956, 9680, 37584, 53938, 43344,
          46423, 27808, 46416, 86869, 19872, 42448, 83315, 21200, 43432, 59728,
          27296, 44710, 43856, 19296, 43748, 42352, 21088, 62051, 55632, 23383,
          22176, 38608, 19925, 19152, 42192, 54484, 53840, 54616, 46400, 46496,
          103846, 38320, 18864, 43380, 42160, 45690, 27216, 27968, 44870, 43872,
          38256, 19189, 18800, 25776, 29859, 59984, 27480, 21952, 43872, 38613,
          37600, 51552, 55636, 54432, 55888, 30034, 22176, 43959, 9680, 37584,
          51893, 43344, 46240, 47780, 44368, 21977, 19360, 42416, 86390, 21168,
          43312, 31060, 27296, 44368, 23378, 19296, 42726, 42208, 53856, 60005,
          54576, 23200, 30371, 38608, 19415, 19152, 42192, 118966, 53840, 54560,
          56645, 46496, 22224, 21938, 18864, 42359, 42160, 43600, 111189, 27936,
          44448,
        ];
        let s,
          t,
          i = 0,
          o = (e - new Date(1900, 0, 31)) / 864e5;
        for (
          this.dayCyl = Math.round(o + 40), this.monCyl = 14, s = 1900;
          s < 2050 && 0 < o;
          s++
        ) {
          let e,
            t = 348,
            a;
          for (e = 32768; 8 < e; e >>= 1) t += n[s - 1900] & e ? 1 : 0;
          (a = 15 & n[s - 1900] ? (65536 & n[s - 1900] ? 30 : 29) : 0),
            (i = t + a),
            (o -= i),
            (this.monCyl += 12);
        }
        for (
          o < 0 && ((o += i), s--, (this.monCyl -= 12)),
            this.year = s,
            this.yearCyl = s - 1864,
            t = 15 & n[s - 1900],
            this.isLeap = !1,
            s = 1;
          s < 13 && 0 < o;
          s++
        )
          (i =
            0 < t && s == 1 + t && 0 == this.isLeap
              ? (s--,
                (this.isLeap = !0),
                15 & n[this.year - 1900]
                  ? 65536 & n[this.year - 1900]
                    ? 30
                    : 29
                  : 0)
              : n[this.year - 1900] & (65536 >> s)
              ? 30
              : 29),
            1 == this.isLeap && s == 1 + t && (this.isLeap = !1),
            (o -= i),
            0 == this.isLeap && this.monCyl++;
        0 == o &&
          0 < t &&
          s == 1 + t &&
          (this.isLeap
            ? (this.isLeap = !1)
            : ((this.isLeap = !0), s--, this.monCyl--)),
          o < 0 && ((o += i), s--, this.monCyl--),
          (this.month = Math.round(s)),
          (this.day = Math.round(o + 1));
      },
      getlunar: function (e, t, a) {
        var a = new Date(e, t, a),
          a = new this.mathlunar(a),
          n = [
            "鼠",
            "牛",
            "虎",
            "兔",
            "龙",
            "蛇",
            "马",
            "羊",
            "猴",
            "鸡",
            "狗",
            "猪",
          ],
          s = [
            "日",
            "一",
            "二",
            "三",
            "四",
            "五",
            "六",
            "七",
            "八",
            "九",
            "十",
          ];
        let i = "";
        var o = a.month,
          l = a.day,
          a =
            (a.monCyl,
            a.dayCyl,
            ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"]),
          r = [
            "子",
            "丑",
            "寅",
            "卯",
            "辰",
            "巳",
            "午",
            "未",
            "申",
            "酉",
            "戌",
            "亥",
          ];
        let c = "";
        let u = o.isLeap ? "闰" : "";
        switch (
          ((u +=
            1 == o ? "正月" : 10 < o ? "十" + s[o - 10] + "月" : s[o] + "月"),
          l)
        ) {
          case 10:
            u += "初十";
            break;
          case 20:
            u += "二十";
            break;
          case 30:
            u += "三十";
            break;
          default:
            u =
              (u += ["初", "十", "廿", "卅", "　"][Math.floor(l / 10)]) +
              s[l % 10];
        }
        return (
          (c =
            t < o - 1
              ? ((i = n[(e - 5) % 12] + "年"),
                a[(e - 1900 + 35) % 10] + r[(e - 1900 + 35) % 12] + "年")
              : ((i = n[(e - 4) % 12] + "年"),
                a[(e - 1900 + 36) % 10] + r[(e - 1900 + 36) % 12] + "年")),
          1 == o && 1 == l && INFORMATION.singleday(i + "吉祥！", !1),
          '<b class="lunary">' + c + '</b><b class="lunard">' + u + "</b>"
        );
      },
      getfestival: function (e, t, a, n, s) {
        var i = "",
          o = [
            0, 21208, 42467, 63836, 85337, 107014, 128867, 150921, 173149,
            195551, 218072, 240693, 263343, 285989, 308563, 331033, 353350,
            375494, 397447, 419210, 440795, 462224, 483532, 504758,
          ],
          l = [
            "小寒",
            "大寒",
            "立春",
            "雨水",
            "惊蛰",
            "春分",
            "清明",
            "谷雨",
            "立夏",
            "小满",
            "芒种",
            !1,
            "小暑",
            "大暑",
            "立秋",
            "处暑",
            "白露",
            "秋分",
            "寒露",
            "霜降",
            "立冬",
            "小雪",
            "大雪",
            !1,
          ],
          r = [
            "0101 春节",
            "0115 元宵节",
            "0202 龙抬头",
            "0303 风筝节",
            "0505 端午节",
            "0707 七夕",
            "0715 中元节",
            "0815 中秋节",
            "0909 重阳节",
            "1001 寒衣节",
            "1015 下元节",
            "1208 腊八节",
            "1223 小年",
            "0100 除夕",
          ],
          c = [
            "0101*元旦",
            "0202 世界湿地日",
            "0214*情人节",
            "0301 海豹日",
            "0303 爱耳日",
            "0308*女人节",
            "0312*植树节",
            "0314 白色情人节",
            "0315 消费者权益日",
            "0317 世界航海日",
            "0321 世界森林日",
            "0322 世界水日",
            "0401*愚人节",
            "0407 世界卫生日",
            "0422 世界地球日",
            "0423 世界图书和版权日",
            "0426 世界知识产权日",
            "0501*劳动节",
            "0504*青年节",
            "0508 世界微笑日",
            "0512 防震减灾日&nbsp;&nbsp;国际护士节",
            "0517 世界电信日",
            "0518 国际博物馆日",
            "0531 世界无烟日",
            "0601*儿童节",
            "0605 世界环境日",
            "0606 全国爱眼日",
            "0611 中国人口日",
            "0620 世界难民日",
            "0623 奥林匹克日",
            "0625 全国土地日",
            "0626 国际禁毒日",
            "0701 建党节",
            "0711 世界人口日",
            "0801*建军节",
            "0806 国际电影节",
            "0808 全民健身日",
            "0903 抗战胜利日",
            "0910*教师节",
            "0914 世界清洁地球日",
            "0916 国际臭氧层保护日",
            "0918 九一八事变纪念日",
            "0920 全国爱牙日",
            "0921 国际和平日",
            "0927 世界旅游日",
            "1001*国庆节",
            "1004 世界动物日",
            "1008 世界视觉日",
            "1009 世界邮政日",
            "1010 辛亥革命纪念日",
            "1014 世界标准日",
            "1016 世界粮食日",
            "1017 世界消除贫困日",
            "1024 联合国日&nbsp;&nbsp;程序员节",
            "1028 世界男性健康日",
            "1031 世界勤俭日",
            "1101*万圣节",
            "1108 中国记者节",
            "1109 中国消防宣传日",
            "1114 世界糖尿病日",
            "1111*双十一",
            "1117 国际大学生节",
            "1121 世界问候日",
            "1201 世界艾滋病日",
            "1203 世界残疾人日",
            "1204 中国法制宣传日",
            "1207 国际民航日",
            "1209 世界足球日",
            "1210 世界人权日",
            "1213 国家公祭日",
            "1221 国际篮球日",
            "1224*平安夜",
            "1225*圣诞节",
            "1229 国际生物多样性日",
          ];
        if (s) {
          for (let e = 0; e < s.length; e++) c.push(s[e]);
          c.sort();
        }
        var u = new Date(e, t, a),
          d = new this.mathlunar(u);
        let m, _, E, f, p, T, g;
        for (g in ((p = new Date(
          31556925974.7 * (e - 1900) +
            6e4 * o[2 * t + 1] +
            Date.UTC(1900, 0, 6, 2, 5)
        )),
        (T = p.getUTCDate()) == a && (m = l[2 * t + 1]),
        (p = new Date(
          31556925974.7 * (e - 1900) +
            6e4 * o[2 * t] +
            Date.UTC(1900, 0, 6, 2, 5)
        )),
        (T = p.getUTCDate()) == a && (m = l[2 * t]),
        n || 11 != t || 13 != a || addClass(body, "griz"),
        4 == t && a == this.calforei(e, t, 2, 0) && (_ = "母亲节"),
        5 == t && a == this.calforei(e, t, 3, 0) && (_ = "父亲节"),
        10 == t && a == this.calforei(e, t, 4, 4) && (_ = "感恩节"),
        c))
          c[g].match(/^(\d{2})(\d{2})([\s\*])(.+)$/) &&
            ((p = Number(RegExp.$1) - (t + 1)),
            (T = Number(RegExp.$2) - a),
            0 == p) &&
            0 == T &&
            (n
              ? "*" == RegExp.$3.toString() && (E = RegExp.$4)
              : E
              ? (E += "&nbsp;&nbsp;" + RegExp.$4)
              : (E = RegExp.$4));
        for (g in r)
          r[g].match(/^(\d{2})(.{2})([\s\*])(.+)$/) &&
            ((p = Number(RegExp.$1) - d.month),
            (T = Number(RegExp.$2) - d.day),
            0 == p) &&
            0 == T &&
            (f = RegExp.$4);
        return (
          0 == t && 1 == a && INFORMATION.singleday(e + " Happy New Year!", !1),
          (i += m ? m + "&nbsp;&nbsp;" : "") +
            (_ ? _ + "&nbsp;&nbsp;" : "") +
            (E ? E + "&nbsp;&nbsp;" : "") +
            (f ? f + "&nbsp;&nbsp;" : "")
        );
      },
      calforei: function (e, t, a, n) {
        let s = 0;
        var i = new Date();
        let o;
        i.setFullYear(e, t, 1), (o = i.getDay());
        for (let e = 0; e < 29; e++) {
          if ((o = 6 < o ? 0 : o) == n && ++s == a) return e + 1;
          o++;
        }
      },
      writecalendar: function (e, t) {
        let a = "",
          n = "",
          s;
        var i = e.getFullYear(),
          o = e.getMonth(),
          l = e.getDate(),
          r = e.getDay();
        let c = e.getHours(),
          u = e.getMinutes(),
          d = e.getSeconds();
        var m, _, E, f, p, T, g;
        let v =
            ["Sun", "Mon", "Tues", "Wednes", "Thurs", "Fri", "Satur"][r] +
            "day&nbsp;&nbsp;",
          h = i + "." + (o + 1) + "." + l + "&nbsp;&nbsp;";
        "user" != t &&
          ((r = this.getfestival(i, o, l, !1, t)),
          (m = this.getlunar(i, o, l)),
          (p = new Date()),
          (_ = new Date()),
          (g = T = ""),
          p.setFullYear(i, o, l),
          p.setHours(c, u, d),
          p.setDate(e.getDate() + 1),
          (E = p.getFullYear()),
          (f = p.getMonth()),
          (p = p.getDate()),
          _.setFullYear(i, o, l),
          _.setHours(c, u, d),
          _.setDate(e.getDate() + 2),
          (o = _.getFullYear()),
          (l = _.getMonth()),
          (e = _.getDate()),
          (T = this.getfestival(E, f, p, !0, t)),
          (g = this.getfestival(o, l, e, !0, t)),
          "" != r && (a += '<b class="bacc_b">' + r + "</b>"),
          "" != T && (a += "<b><em>明天</em>" + T + "</b>"),
          "" != g && (a += "<b><em>后天</em>" + g + "</b>"),
          (s = "Copyright&nbsp;&nbsp;&copy;&nbsp;&nbsp;" + i),
          (s += "&nbsp;&nbsp;aur.one<b>&nbsp;&nbsp;All Rights Reserved</b>"),
          (copy_calend.innerHTML = a + m),
          (copy_notice.innerHTML = s),
          (copy_calend.style.display = "inline-block"),
          (statement.style.opacity = "1")),
          3 == UE_NERO.ue.c.ski &&
            (6 <= c && c <= 18 ? checkScheme(0) : checkScheme(1)),
          d < 10 && (d = "0" + d.toString()),
          u < 10 && (u = "0" + u.toString()),
          c < 10 && (c = "0" + c.toString()),
          clearInterval(refresh_time_timing),
          (refresh_time_timing = setInterval(function () {
            d++,
              refresh_times++,
              59 < (d = d < 10 ? "0" + d.toString() : d) &&
                ((d = "00"), ++u < 10) &&
                (u = "0" + u.toString()),
              23 <
                (c =
                  59 < u && ((u = "00"), ++c < 10) ? "0" + c.toString() : c) &&
                (c = "00"),
              (n = h + v + "<b>" + c + ":" + u + ":" + d + "</b>"),
              19 < refresh_times &&
                (clearInterval(refresh_time_timing),
                (airnotice.className = "airnotice note ftf-tra"),
                (n = h + v)),
              airnotice_time && (air_time.innerHTML = n);
          }, 1e3)),
          (n = h + v + "<b>" + c + ":" + u + ":" + d + "</b>"),
          (air_time.innerHTML = n);
      },
    };
  let INSPECT = { offline: function (e) {} };
  const INPUTKEYTEXT = {
      savehist: function (t) {
        var e = localStorage.getItem("Aursear");
        let a = [];
        if (t) {
          if (e) {
            (a = JSON.parse(e)).unshift(t);
            for (let e = 0; e < a.length; e++)
              0 < e && t == a[e] && a.splice(e, 1), 9 < e && a.pop();
          } else a[0] = t;
          localStorage.setItem("Aursear", JSON.stringify(a));
        }
        (inwsmcan.style.display = "none"), (keylists_se = 0);
      },
      writehist: function () {
        var e = localStorage.getItem("Aursear");
        if (e) {
          var t = JSON.parse(e),
            a = t.length,
            n = [];
          for (let e = 0; e < a && !(e > hints_rows - 2); e++)
            n[e] = '<li class="bacch_b">' + t[e] + "</li>";
          0 < a &&
            ((inwsmcan.innerHTML =
              '<ul id="keylist" class="keylist">' +
              n.join("") +
              '<li id="clear" class="clear forch">清空搜索记录</li></ul>'),
            (inwsmcan.style.display = "block"));
        }
      },
      getHint: function () {
        let e;
        let t;
        var a = inwsM.value.replace(reg_spc, "$1").replace(reg_isp, " "),
          a =
            (e = a
              ? "c=" + encodeURIComponent(a)
              : "u=" + NOW_SO.u.split("-")[0].toString()) +
            "&v=" +
            now_ver +
            loa_sta;
        ((t = new XMLHttpRequest()).onreadystatechange = function () {
          var e;
          4 == t.readyState &&
            200 == t.status &&
            ((e = t.responseText)
              ? ((e = JSON.parse(e)), INPUTKEYTEXT.writeHint(e, "B"))
              : ((inwsmcan.style.display = "none"), (keylists_se = 0)));
        }),
          t.open("POST", INITURL.s, !0),
          t.setRequestHeader(
            "Content-type",
            "application/x-www-form-urlencoded"
          ),
          t.send(a);
      },
      writeHint: function (t, e) {
        var a = [],
          n = t.length;
        if ("A" == e) {
          var s = t.AS.Results[0].Suggests,
            i = s.length;
          for (let e = 0; e < i && !(e > hints_rows - 1); e++)
            a[e] = '<li class="bacch_b">' + s[e].Txt + "</li>";
        } else if ("B" == e)
          for (let e = 0; e < n && !(e > hints_rows - 1); e++)
            a[e] = '<li class="bacch_b">' + t[e] + "</li>";
        (inwsmcan.innerHTML =
          '<ul id="keylist" class="keylist">' + a.join("") + "</ul>"),
          (inwsmcan.style.display = "block");
      },
      convertPinyin: function (a) {
        let n = "";
        var e = a.length,
          s = /[a-zA-Z0-9\- ]/;
        for (let t = 0; t < e; t++) {
          let e = !1;
          var i,
            o = a.substr(t, 1);
          for (i in serial_piny)
            if (-1 != serial_piny[i].indexOf(o)) {
              e = i;
              break;
            }
          s.test(o) ? (n += o) : e && (n += e);
        }
        return (
          "" !=
            (n = n.replace(
              /[\ |\~|\`|\!|\@|\#|\$|\%|\^|\&|\*|\(|\)|\-|\_|\+|\=|\||\\|\[|\]|\{|\}|\;|\:|\'|\'|\,|\<|\.|\>|\/|\?]/g,
              ""
            )) && n
        );
      },
    },
    INTERFACE = {
      cancelDefault: function (e) {
        (e = e || window.event) && e.preventDefault
          ? e.preventDefault()
          : (e.returnValue = !1),
          ie ? (e.cancelBubble = !0) : e.stopPropagation();
      },
      writeRightmenu: function (a, e, n) {
        let t = 2,
          s = 2,
          i = 96,
          o = 96;
        var l,
          r,
          c,
          u,
          d,
          m = a.length;
        let _;
        for (let e = 0; e < m; e++)
          a[e].n.length > t && ((t = a[e].n.length), (i = 16 * t + 72));
        rightmenu.innerHTML = "";
        for (let t = 0; t < m; t++) {
          if (
            ((l = document.createElement("li")).setAttribute(
              "data-index",
              a[t].c
            ),
            (l.className = "baccllh"),
            (l.innerText = a[t].n),
            a[t].t)
          ) {
            let e = a[t].t;
            mac &&
              (e = (e = e.replace(/Ctrl/g, "Command")).replace(
                /Alt/g,
                "Option"
              )),
              ((r = document.createElement("span")).innerText = e),
              l.appendChild(r);
          }
          if (a[t].s) {
            (l.className = "minor baccllh"),
              (c = a[t].s.length),
              ((_ = document.createElement("ul")).className = "menudary");
            for (let e = 0; e < c; e++)
              (u = document.createElement("li")).setAttribute(
                "data-index",
                a[t].s[e].c
              ),
                a[t].s[e].c == input_tran
                  ? (u.className = "baccllh active")
                  : (u.className = "baccllh"),
                (u.innerText = a[t].s[e].n),
                a[t].s[e].n.length > s &&
                  ((s = a[t].s[e].n.length), (o = 16 * s + 64)),
                a[t].s[e].t &&
                  (((d = document.createElement("span")).innerText =
                    a[t].s[e].t),
                  u.appendChild(d)),
                a[t].s[e].a || (l.className = "unava"),
                _.appendChild(u);
            l.appendChild(_),
              (_.style.width = o + "px"),
              (_.style.left = i + "px"),
              i + o + e > screen_size[0] && (_.style.left = 0 - o + "px"),
              40 * (c + t) + n - 64 > screen_size[1] &&
                (_.style.top = -(40 * (c - 1) - 32) + "px"),
              addEvent(l, "mouseenter", function (e) {
                clearTimeout(hide_menudary_dy), (_.className = "menudary show");
              }),
              addEvent(l, "mouseleave", function (e) {
                clearTimeout(hide_menudary_dy),
                  (hide_menudary_dy = setTimeout(function () {
                    (_.className = "menudary hide"),
                      clearTimeout(hide_menudary_dy);
                  }, 800));
              });
          }
          a[t].a || (l.className = "unava"), rightmenu.appendChild(l);
        }
        (rightmenu.style.width = i + "px"),
          (e = i + e > screen_size[0] ? screen_size[0] - i : e) < 16 &&
            (e = 16),
          (n =
            40 * m + n + 16 > screen_size[1]
              ? screen_size[1] - 40 * m - 16
              : n) < 16 && (n = 16),
          (rightmenu.style.left = e - 8 + "px"),
          (rightmenu.style.top = n - 8 + "px"),
          (overmask.className = "overmask menu");
      },
      setUp: function (e, t) {
        let a, n;
        var s = NOW_SO.u.split("-")[0],
          i = inwsM.value
            .toLowerCase()
            .replace(reg_spc, "$1")
            .replace(reg_isp, " ");
        if (6 == e)
          (UE_NERO.ue.x = 1),
            (innermotif.className = "innermotif down"),
            (includeall.className = "includeall down"),
            foc_lastnav_n && NAVIGATE.focNavsite(!0),
            inwsM.blur(),
            USERCONF.saveUSERUE();
        else if (7 == e)
          (UE_NERO.ue.x = 2),
            (innersortlist.scrollTop = 0),
            (innermotif.className = "innermotif right"),
            (includeall.className = "includeall right"),
            inwsM.blur();
        else if (9 == e)
          (UE_NERO.ue.x = 0),
            (innermotif.className = "innermotif"),
            (includeall.className = "includeall"),
            USERCONF.saveUSERUE(),
            inwsM.focus();
        else if (11 == e)
          (UE_NERO.ue.t = 1),
            (RMC_O[1] = RMC_WS[1]),
            (RMC_S[2] = RMC_WS[1]),
            (RMC_N[2] = RMC_WS[1]),
            (workslist.className = "workslist show"),
            (topcontrwor.className = "topcontrwor point forch"),
            (filtermask.className = "filtermask narr"),
            (includeall.style.width = "75%"),
            (topengine.className = "topengine narr"),
            (configure.className = "configure narr"),
            USERCONF.saveUSERUE();
        else if (12 == e)
          (UE_NERO.ue.t = 0),
            (RMC_O[1] = RMC_WS[0]),
            (RMC_S[2] = RMC_WS[0]),
            (RMC_N[2] = RMC_WS[0]),
            (workslist.className = "workslist hide"),
            (topcontrwor.className = "topcontrwor forch"),
            (filtermask.className = "filtermask"),
            (includeall.style.width = "100%"),
            setTimeout(function () {
              topengine.className = "topengine";
            }, 800),
            (configure.className = "configure"),
            USERCONF.saveUSERUE();
        else if (13 == e)
          (RMC_O[1] = RMC_WS[1]),
            (RMC_S[2] = RMC_WS[1]),
            (RMC_N[2] = RMC_WS[1]),
            (candidate.className = "candidate show"),
            (topcontrwor.className = "topcontrwor point forch"),
            (filtermask.className = "filtermask narr"),
            (includeall.style.width = "75%"),
            (topengine.className = "topengine narr"),
            (configure.className = "configure narr"),
            SUPEREDIT.loadLast();
        else if (14 == e)
          (RMC_O[1] = RMC_WS[0]),
            (RMC_S[2] = RMC_WS[0]),
            (RMC_N[2] = RMC_WS[0]),
            (candidate.className = "candidate hide"),
            (topcontrwor.className = "topcontrwor forch"),
            (filtermask.className = "filtermask"),
            (includeall.style.width = "100%"),
            setTimeout(function () {
              topengine.className = "topengine";
            }, 800),
            (configure.className = "configure");
        else if (16 == e)
          (find_end = !1),
            (nav_find.value = ""),
            UE_NERO.ue.v.vw
              ? ((RMC_N[1].a = !1),
                (navselect.className = "navselect labelmar"),
                (navdetail.className = "navdetail labelmar"),
                NAVIGATE.setNavlabe())
              : ((RMC_N[1].a = !0),
                (navselect.className = "navselect levelmar"),
                (navdetail.className = "navdetail levelmar"),
                NAVIGATE.setNavlevel()),
            nav_find.blur();
        else {
          if (21 == e)
            return (
              (u = inwsM.value),
              (c = inwsT.value),
              ("" != u || "" != c) &&
                ((inwsM.value = ""),
                (inwsT.value = ""),
                CORE.censorKeyword(),
                !0)
            );
          if (22 == e)
            return (
              NOW_SO.u.split("-")[0] != UE_NERO.ue.o &&
              ((u = AUTO.engLoca(UE_NERO.ue.o)),
              CORE.speEngine(u[0], u[1], u[2]),
              !0)
            );
          if (23 == e)
            "" != i &&
              (copyText(inwsM.value)
                ? INTERFACE.pusMess("已复制到剪贴板！", "carry", 4)
                : INTERFACE.pusMess(
                    "复制失败，请全选使用 Ctrl + C 复制",
                    "fail",
                    4
                  ));
          else if (24 == e)
            localStorage.removeItem("Aursear"),
              (inwsmcan.style.display = "none"),
              (keylists_se = 0),
              inwsM.focus();
          else if (25 == e) {
            let e;
            "0100" == s && "" == i
              ? ((e = ASE),
                INTERFACE.pusMess(
                  "该功能可将当前选中的引擎和关键词生成为一个链接",
                  "caption",
                  6
                ))
              : (e =
                  "" == i
                    ? ASE + "?u=" + s
                    : ASE + "?u=" + s + "&c=" + encodeURIComponent(i)),
              copyText(e)
                ? INTERFACE.pusMess("链接已复制到剪贴板！", "carry", 4)
                : INTERFACE.pusMess("链接复制失败", "fail", 4);
          } else if (26 == e)
            (a = UE_NERO.ue.v.vm),
              (n = UE_NERO.ue.v.vs[a]),
              a < 10 && (a = "0" + a.toString()),
              n < 10 && (n = "0" + n.toString()),
              copyText(ASE + "?n=" + a + n)
                ? INTERFACE.pusMess("链接已复制到剪贴板！", "carry", 4)
                : INTERFACE.pusMess("链接复制失败", "fail", 4);
          else if (31 == e)
            window.open("https://jq.qq.com/?_wv=1027&k=HfP9uqFC", openw[2]),
              USERCONF.upAtion("SEL", "groupchat");
          else if (32 == e)
            window.open(
              "http://mail.qq.com/cgi-bin/qm_share?t=qm_mailme&email=TSgkIygsDTw8Yy4iIA",
              openw[2]
            ),
              USERCONF.upAtion("SEL", "mail");
          else if (33 == e)
            window.open("https://support.qq.com/products/29062", openw[2]),
              USERCONF.upAtion("SEL", "feedback");
          else if (41 == e) {
            var o = ITEM_ON.length;
            for (let e = 0; e < o; e++)
              (5 != ITEM_ON[e].u && 6 != ITEM_ON[e].u) || (ITEM_ON[e].c = !1);
            TODOITEM.readTodolist(!0);
          } else if (42 == e) {
            var l = ITEM_ON.length;
            for (let e = 0; e < l; e++) ITEM_ON[e].o = !1;
            TODOITEM.readTodolist(!0);
          } else if (43 == e) {
            var r = ITEM_ON.length;
            for (let e = 0; e < r; e++) ITEM_ON[e].o = !0;
            TODOITEM.readTodolist(!0);
          } else if (55 == e) {
            var c = window.location.href;
            window.location.replace(c);
          } else if (56 == e) {
            var u = confirm("记录和设置清除后无法恢复，确定继续？");
            let e = window.location.href;
            u &&
              (localStorage.clear(),
              (overmask.className = "overmask mask"),
              (overload.className = "overload wait"),
              setTimeout(function () {
                window.location.replace(e);
              }, 4e3));
          } else if (61 == e) {
            let e;
            (s = localStorage.getItem("Aurmeet")), (i = JSON.parse(s) || []);
            i[4] ||
              ((i[4] = ~~guidecourse.getAttribute("data-ver")),
              localStorage.setItem("Aurmeet", JSON.stringify(i)));
            let t = document.createElement("span");
            (t.className = "quickstart twinkle"),
              (t.innerHTML = "<i>\\</i>快速上手"),
              copyright.appendChild(t),
              addEvent(t, "click", function (e) {
                remEle(t), INFORMATION.introduction("ative");
              }),
              (guidecourse.className = "guidecourse hide"),
              (e = setTimeout(function () {
                (guidecourse.style.display = "none"), clearTimeout(e);
              }, 200));
          } else
            62 == e
              ? (addClass(filtermask, "setshow"),
                clearTimeout(live_demo_delay),
                (live_demo_delay = setTimeout(function () {
                  remClass(filtermask, "setshow"),
                    clearTimeout(live_demo_delay);
                }, t)))
              : 63 == e
              ? ((consortlist.className = "consortlist setshow"),
                clearTimeout(live_demo_delay),
                (live_demo_delay = setTimeout(function () {
                  (consortlist.className = "consortlist"),
                    clearTimeout(live_demo_delay);
                }, t)))
              : 64 == e
              ? ((innermotif.className = "innermotif setshow"),
                (consortlist.className = "consortlist setshow"))
              : 65 == e &&
                (clearTimeout(live_demo_delay),
                (live_demo_delay = setTimeout(function () {
                  2 == UE_NERO.ue.x &&
                    (innermotif.className = "innermotif right"),
                    (consortlist.className = "consortlist"),
                    clearTimeout(live_demo_delay);
                }, t)));
        }
      },
      openElement: function (t, a, n, s) {
        let i;
        i = setInterval(function () {
          let e = 0;
          a < n
            ? (e = Math.ceil((n - a) / 8))
            : n < a && (e = Math.floor((n - a) / 8)),
            (a += e),
            (t.style.height = a + "px"),
            n - 1 < a &&
              a < n + 1 &&
              (clearInterval(i), 0 == n) &&
              s &&
              (t.style.display = "none");
        }, 16);
      },
      cutListSwitch: function (e) {
        if (getId("keylist")) {
          var a = getIat("keylist", "li");
          let t = a.length;
          for (let e = 0; e < a.length; e++) "clear" == a[e].id && t--;
          if (1 == e) {
            (!keylists_se || --keylists_se < 1 || keylists_se > t) &&
              (keylists_se = t);
            for (let e = 0; e < t; e++)
              e == keylists_se - 1
                ? (a[e].className = "bacch_b bacc_b")
                : (a[e].className = "bacch_b");
          } else if (2 == e) {
            (!keylists_se || ++keylists_se < 1 || keylists_se > t) &&
              (keylists_se = 1);
            for (let e = 0; e < t; e++)
              e == keylists_se - 1
                ? (a[e].className = "bacch_b bacc_b")
                : (a[e].className = "bacch_b");
          } else if (3 == e)
            (inwsM.value = a[keylists_se - 1].innerText),
              (inwsmcan.style.display = "none"),
              (keylists_se = 0),
              CORE.censorKeyword(),
              input_tran && CORE.toTranslate();
          else if (0 == e) {
            for (let e = 0; e < t; e++) a[e].className = "bacch_b";
            keylists_se = 0;
          }
        }
      },
      pusMess: function (a, n, s, i, o, l) {
        let r,
          t,
          c,
          u = 0;
        a.length;
        var d,
          e,
          m = getId("facemessage");
        let _ = document.createElement("p"),
          E,
          f,
          p = 0,
          T = 0;
        if (!a) return !1;
        {
          let e = "talk" == n ? 10 : 8;
          d = a.length;
          for (let e = 0; e < d; e++)
            0 != (65280 & a.charCodeAt(e)) && u++, u++;
          (r = u * e + 80), (t = "width: " + r + "px;"), (_.innerText = a);
        }
        if (
          (n &&
            ("rcolor" == n
              ? (t += " background-color: " + INTERFACE.randomCo() + ";")
              : reg_hex.test(n) || reg_rgb.test(n)
              ? (t += " background-color: " + n + ";")
              : addClass(_, "talk" == n ? "ftf-tra" : n)),
          reg_num.test(s) || (s = 5),
          /^\-|^\+/.test(i) &&
            ((n = i.substr(0, 1)),
            (e = Number(i.substr(1))),
            "+" == n ? (p += e) : (p -= e),
            (i = !1)),
          /^\-|^\+/.test(o) &&
            ((n = o.substr(0, 1)),
            (e = Number(o.substr(1))),
            "+" == n ? (T += e) : (T -= e),
            (o = !1)),
          reg_num.test(i) || (i = Math.round(screen_size[0] / 2 - r / 2) + p),
          reg_num.test(o) || (o = Math.round(screen_size[1] / 2 - 24) + T),
          reg_num.test(i) && reg_num.test(o))
        ) {
          let e = 8,
            t = screen_size[0] - (r + 8);
          n = screen_size[1] - 56;
          2 == l || 3 == l
            ? ((i -= r),
              (t = screen_size[0] - 8),
              3 == l && ((e = 56), (o -= 48)))
            : 4 == l && ((e = 56), (o -= 48)),
            i < 8 && (i = 8),
            o < e && (o = e),
            i > t && (i = t),
            n < o && (o = n);
        }
        (t = t + " left: " + i + "px; top: " + o + "px;"),
          l &&
            (((e = ["24px", "24px", "24px", "24px"])[~~(l - 1)] = "0px"),
            (t += " border-radius: " + e.join(" ") + ";")),
          _.setAttribute("style", t),
          (c = m.getElementsByTagName("p"));
        for (let t = 0; t < c.length; t++)
          if (c[t].innerText == a) {
            let e = setTimeout(function () {
              remEle(c[t]), clearTimeout(e);
            }, 600);
            addClass(_, "warning"), (s += 5);
          }
        (E = setTimeout(function () {
          addClass(_, "hide"),
            (f = setTimeout(function () {
              remEle(_), clearTimeout(f);
            }, 600)),
            clearTimeout(E);
        }, 1e3 * s)),
          m.appendChild(_),
          addEvent(_, "click", function (e) {
            addClass(_, "hide"),
              (f = setTimeout(function () {
                remEle(_), clearTimeout(f);
              }, 600));
          });
      },
      facialclassName: function (e) {
        var e = e.split("-"),
          t = e[0],
          a = e[1],
          n = body.className.split(" ");
        let s = !1;
        if (0 < n.length && "" != n[0])
          for (let e = 0; e < n.length; e++)
            n[e].split("-")[0] == t && ((n[e] = t + "-" + a), (s = !0));
        else (n[0] = t + "-" + a), (s = !0);
        s || n.push(t + "-" + a), (e = n.join(" ")), (body.className = e);
      },
      setHSL: function (e, t, a, n) {
        let s = 0;
        var i, o;
        let l;
        (Array.isArray(e) || reg_val.test(e)) &&
          (Array.isArray(e)
            ? ((i = Math.abs(148 - e[0])),
              (o = Math.abs(120 - e[1])),
              (i = Math.sqrt(Math.pow(i, 2) + Math.pow(o, 2))),
              (o = Math.acos(o / i)),
              (s = Math.floor(180 / (Math.PI / o))),
              148 < e[0] && 120 < e[1] && (s = 180 - s),
              148 == e[0] && 120 < e[1] && (s = 180),
              148 < e[0] && 120 == e[1] && (s = 90),
              e[0] < 148 && 120 < e[1] && (s = 180 + s),
              e[0] < 148 && 120 == e[1] && (s = 270),
              e[0] < 148 && e[1] < 120 && (s = 360 - s))
            : (s = Number(e)),
          180 < Math.abs(s - INC.h) &&
            (s > INC.h ? input_hue_turns-- : input_hue_turns++),
          (i = s + 360 * input_hue_turns),
          (INC.h = s),
          (huepoint.style.webkitTransform = "rotate(" + i + "deg)"),
          (huepoint.style.mozTransform = "rotate(" + i + "deg)"),
          (huepoint.style.msTransform = "rotate(" + i + "deg)"),
          (huepoint.style.oTransform = "rotate(" + i + "deg)"),
          (huepoint.style.transform = "rotate(" + i + "deg)")),
          reg_val.test(t) &&
            (200 < (t = t < 0 ? 0 : t) && (t = 200),
            (INC.s = Math.round(t / 2)),
            194 < (t = t < 6 ? 6 : t) && (t = 194),
            (satpoint.style.webkitTransform = "translateX(" + t + "px)"),
            (satpoint.style.mozTransform = "translateX(" + t + "px)"),
            (satpoint.style.msTransform = "translateX(" + t + "px)"),
            (satpoint.style.oTransform = "translateX(" + t + "px)"),
            (satpoint.style.transform = "translateX(" + t + "px)")),
          reg_val.test(a) &&
            (200 < (a = a < 0 ? 0 : a) && (a = 200),
            (INC.l = Math.round(a / 2)),
            194 < (a = a < 6 ? 6 : a) && (a = 194),
            (ligpoint.style.webkitTransform = "translateX(" + a + "px)"),
            (ligpoint.style.mozTransform = "translateX(" + a + "px)"),
            (ligpoint.style.msTransform = "translateX(" + a + "px)"),
            (ligpoint.style.oTransform = "translateX(" + a + "px)"),
            (ligpoint.style.transform = "translateX(" + a + "px)")),
          (o = "hsl(" + INC.h + ", " + INC.s + "%, " + INC.l + "%)"),
          l,
          (e = ("hsl(" + INC.h + ", 100%, 50%)").toHex()),
          (l = (n || o).toHex()),
          (huepoint.style.backgroundColor = l),
          (satwheel.style.backgroundColor = e),
          (ligwheel.style.backgroundColor = e),
          (sort_icv.value = l.toUpperCase().replace(/#/, "")),
          (co_mix = l);
      },
      cutTheme: function (e, t) {
        var a = getId("themecolor"),
          a =
            (a && remEle(a),
            reg_num.test(e) &&
              (e = (
                DARK
                  ? ["#495867", "#608000", "#7f4500", "#6d1f00", "#00467f"]
                  : ["#273645", "#85b200", "#ff9500", "#ef4a00", "#0080ff"]
              )[e]),
            this.colorBrightness(e, "-20")),
          n = this.colorBrightness(e, "40"),
          s = this.colorBrightness(e, "90"),
          i = e.toRgb("0"),
          o = e.toRgb("0.1"),
          l = " color: " + e + " !important; ",
          r = " background-color: " + e + " !important; ",
          c = " background-color: " + n + " !important; ",
          u = " background-color: " + s + " !important; ",
          d = " border-color: " + e + " !important; ",
          o = " background-color: " + o + "; ",
          m = ".forc {" + l + "} ",
          _ =
            ((m =
              m +
              (".forch:hover {" + l + "} ") +
              (".forch.hover {" + l + "} ")),
            ".bacc {" + r + "} "),
          E =
            ((_ =
              (_ =
                (_ =
                  _ + (".bacch:hover {" + r + "} ") + (".baccl {" + c + "} ")) +
                (".baccll {" + u + "} ") +
                (".bacclh:hover {" + c + "} ")) +
              (".baccllh:hover {" + u + "} ") +
              (".baccllh.hover {" + u + "} ")),
            ".bacc_b::before {" + r + "} "),
          f =
            ((E += ".bacch_b:hover::before {" + r + "} "),
            ".borc {" + d + "} "),
          p =
            ".navigation>.navselect>input#nav_find:hover { color: " +
            e +
            "; } ",
          u =
            ((p =
              (p =
                (p =
                  (p +=
                    ".navigation>.navselect>input#nav_find:focus+label::after { " +
                    d +
                    "; } ") +
                  (".navigation>.navdetail>.navfid>li.forch:hover>b { " +
                    r +
                    "} ") +
                  (".navigation>.navselect.levelmar>.selectway>.selectlevel::before {" +
                    l +
                    "} ")) +
                (".navigation>.navselect.labelmar>.selectway>.selectlabel::before {" +
                  l +
                  "} ") +
                (".brands:hover>p {" + l + "} ")) +
              (".brands.hover::before {" + c + "} ") +
              (".brands:hover::before {" + u + "} ")),
            ".engines>ul>li>input[type=radio]:checked + label::before {" +
              d +
              "} "),
          c =
            ((u =
              (u =
                (u =
                  u +
                  (".engines>ul>li>input[type=radio] + label.star::before {" +
                    c +
                    "} ") +
                  (".engines>ul>li>input[type=radio]:checked + label.star::before {" +
                    (" border-color: " + n + " !important; ") +
                    r +
                    "} ")) +
                (".topengine>.searcore>.entryfield>.implement>div {border-color: " +
                  e +
                  "; " +
                  (" background-color: " + i + "; ") +
                  "} ") +
                (".topengine>.searcore>.entryfield>.implement>div:hover {" +
                  o +
                  "} ")) +
              (".topengine>.searcore>.entryfield>.implement>div.hover {" +
                o +
                "} ") +
              (".workitem>div.close>.noteset>li:nth-child(3):hover { background-color: " +
                s +
                "} ")),
            "::-webkit-selection {" + r + "} "),
          c =
            (c += "::-moz-selection {" + r + "} ") +
            ("::-ms-selection {" + r + "} ") +
            ("::selection {" + r + "} ");
        let T = "";
        t &&
          (T =
            (T =
              (T =
                (T =
                  (T =
                    (T =
                      (T =
                        (T =
                          (T =
                            (T =
                              (T =
                                (T =
                                  (T =
                                    (T =
                                      ".innersortlist>li>p:hover {" +
                                      l +
                                      "} ") +
                                    ".innersortlist>li>p:hover>span {" +
                                    l +
                                    "} .innersortlist>li>nav:hover {" +
                                    l +
                                    "} ") +
                                  ".innersortlist>li>input#sort_tit:focus+label::after { " +
                                  d +
                                  "; } .innersortlist>li>ul#sort_def { " +
                                  d +
                                  "; } ") +
                                ".colorpick>.huepick>.huepoint::after { " +
                                d +
                                "; }.colorpick>.satpick>.satpoint { " +
                                d +
                                "; }") +
                              ".colorpick>.ligpick>.ligpoint { " +
                              d +
                              "; }.innersortlist>#sort_copli>ul.resolve>li#sort_coprd:hover {" +
                              l +
                              "} ") +
                            ".innersortlist>#sort_copli>ul.resolve>li#sort_coprd:hover::before {" +
                            l +
                            "} .innersortlist>li>p>i { background-color: " +
                            e +
                            "} ") +
                          ".innersortlist>li>p:hover>i {" +
                          (" background-color: " + a + " !important; ") +
                          "} .innersortlist>li>input#sort_tit:hover { color: " +
                          e +
                          "; } ") +
                        ".innersortlist>li>.sort_radios>li>input[type=radio]:checked+label::before {" +
                        d +
                        "} .innersortlist>li>.sort_radios>li>input[type=checkbox]:checked+label::before {" +
                        d +
                        "} ") +
                      ".innersortlist>li>.sort_radios>li>label:hover {" +
                      l +
                      "} .innersortlist>li>.sort_radios>li>label:hover>span {" +
                      l +
                      "} ") +
                    ".innersortlist>li>.sort_capacity>span.used::before {" +
                    r +
                    "} .innersortlist>li>.sort_capacity>em>i {" +
                    r +
                    "} ") +
                  ".innersortlist>li>input#sort_tra:hover+label::before, .innersortlist>li>input#sort_tra:hover+label::after {" +
                  l +
                  "} ") +
                ".innersortlist>li>input[type=range]::-webkit-slider-thumb {" +
                d +
                "} .innersortlist>li>input[type=range]::slider-thumb {" +
                d +
                "} ") +
              ".innersortlist>li>input[type=range]::-moz-range-thumb {" +
              d +
              "} .innersortlist>li>input[type=range]::range-thumb {" +
              d +
              "} ") +
            ".innersortlist>li>input[type=range]::-ms-thumb {" +
            d +
            "} .innersortlist>li>input[type=range]::thumb {" +
            d +
            "} "),
          ((n = document.createElement("style")).id = "themecolor"),
          (n.innerText = m + _ + E + f + T + p + u + c),
          document.head.appendChild(n),
          (shuttle.style.backgroundColor = e);
      },
      randomCo: function () {
        let e = { r: 0, g: 0, b: 0 },
          t = parseInt("88", 16),
          a = parseInt("FF", 16);
        for (
          var n, s;
          !(
            510 <=
              (e =
                ((s = a),
                {
                  r: (n = t) + (Math.round(1e3 * Math.random()) % (s - n)),
                  g: n + (Math.round(1e3 * Math.random()) % (s - n)),
                  b: n + (Math.round(1e3 * Math.random()) % (s - n)),
                })).r +
                e.g +
                e.b && e.r + e.g + e.b <= 765
          );

        );
        return "rgb(" + e.r + ", " + e.g + ", " + e.b + ")";
      },
      colorBrightness: function (e, t) {
        let a = !0,
          n = [],
          s = Number(t / 100);
        (s = DARK ? 0 - s : s) < 0 && ((s = Math.abs(s)), (a = !1));
        var i = e
          .toRgb()
          .replace(/(?:\(|\)|rgb|RGB)*/g, "")
          .split(",");
        for (let t = 0; t < i.length; t++) {
          let e = Number(i[t]);
          a
            ? 255 < (e += (255 - e) * s) && (e = 255)
            : (e -= e * s) < 0 && (e = 0),
            n.push(Math.round(e));
        }
        return (n = "rgb(" + n.join(", ") + ")").toHex();
      },
    };
  function onlyOnce(e, t) {}
  function shakeThrottle(e, t, a) {
    new Date();
  }
  function refreshConfile(e, t, a) {
    let n = "";
    var s = getId(t);
    a && (n = "?v=" + now_ver + loa_sta),
      s && body.removeChild(s),
      (s = document.createElement("script")).setAttribute("charset", "UTF-8"),
      (s.id = t),
      (s.src = e + n),
      body.appendChild(s);
  }
  addEvent(window, A7, function (e) {
    // let t;
    // var a = ASN.substr(0, 16),
    //   n = KEY.substr(0, 16),
    //   a =
    //     "id=" +
    //     a +
    //     ("&v=" + now_ver) +
    //     ("&salt=" + loa_sta) +
    //     ("&key=" + n) +
    //     "&sign=" +
    //     A5(a + now_ver + loa_sta + n);
    // ((t = new XMLHttpRequest()).onreadystatechange = function () {
    //   var e;
    //   console.log(e)
    //   4 == t.readyState && 200 == t.status && ((e = t.responseText), alert(e));
    // }),
    //   t.open("POST", "note/aurone.php", !0),
    //   t.setRequestHeader("Content-type", "application/x-www-form-urlencoded"),
    //   t.send(a),
    INITURL = {
      i: "note/",
      a: "note/cont/main.json",
      u: "note/cont/nero.json",
      r: "note/cont/user.json",
      o: "note/cont/cont.json",
      n: "note/carr/hotspot.php",
      e: "note/carr/translate.php",
      w: "note/carr/news.php",
      s: "note/carr/suggest.php",
      v: "note/carr/total.php",
    };
    b_searc = ["https://www.baidu.com/s?ie=utf-8&wd="];
    b_sites = ["https://www.baidu.com/s?ie=utf-8&wd=", "&si=", "&ct="];
    IMPORTANCE.userVisit("aur.one");

    // refreshConfile(
    //   "https://hm.baidu.com/hm.js?8f5618062f0bb44daa9369f0f941cd68",
    //   "baiduhm",
    //   !1
    // );
  });
})(window);
