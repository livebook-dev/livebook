import{a as ee,b as se}from"./chunk-BQJTSJOB.js";import{b as te}from"./chunk-5JLOMFCN.js";import{e as Qt,m as Zt}from"./chunk-47P5NBBB.js";import{N as P,S as Ht,T as zt,U as Wt,V as Xt,W as Kt,X as Jt,Y as qt,Z as x,h as i,j as k}from"./chunk-KRX7QNR4.js";var Nt=function(){var t=i(function($,o,u,n){for(u=u||{},n=$.length;n--;u[$[n]]=o);return u},"o"),e=[1,2],c=[1,3],a=[1,4],d=[2,4],r=[1,9],p=[1,11],g=[1,16],l=[1,17],_=[1,18],T=[1,19],m=[1,32],Y=[1,20],F=[1,21],I=[1,22],f=[1,23],L=[1,24],R=[1,26],V=[1,27],M=[1,28],N=[1,29],w=[1,30],it=[1,31],at=[1,34],nt=[1,35],lt=[1,36],ot=[1,37],J=[1,33],S=[1,4,5,16,17,19,21,22,24,25,26,27,28,29,33,35,37,38,42,45,48,49,50,51,54],ct=[1,4,5,14,15,16,17,19,21,22,24,25,26,27,28,29,33,35,37,38,42,45,48,49,50,51,54],Mt=[4,5,16,17,19,21,22,24,25,26,27,28,29,33,35,37,38,42,45,48,49,50,51,54],Dt={trace:i(function(){},"trace"),yy:{},symbols_:{error:2,start:3,SPACE:4,NL:5,SD:6,document:7,line:8,statement:9,classDefStatement:10,styleStatement:11,cssClassStatement:12,idStatement:13,DESCR:14,"-->":15,HIDE_EMPTY:16,scale:17,WIDTH:18,COMPOSIT_STATE:19,STRUCT_START:20,STRUCT_STOP:21,STATE_DESCR:22,AS:23,ID:24,FORK:25,JOIN:26,CHOICE:27,CONCURRENT:28,note:29,notePosition:30,NOTE_TEXT:31,direction:32,acc_title:33,acc_title_value:34,acc_descr:35,acc_descr_value:36,acc_descr_multiline_value:37,classDef:38,CLASSDEF_ID:39,CLASSDEF_STYLEOPTS:40,DEFAULT:41,style:42,STYLE_IDS:43,STYLEDEF_STYLEOPTS:44,class:45,CLASSENTITY_IDS:46,STYLECLASS:47,direction_tb:48,direction_bt:49,direction_rl:50,direction_lr:51,eol:52,";":53,EDGE_STATE:54,STYLE_SEPARATOR:55,left_of:56,right_of:57,$accept:0,$end:1},terminals_:{2:"error",4:"SPACE",5:"NL",6:"SD",14:"DESCR",15:"-->",16:"HIDE_EMPTY",17:"scale",18:"WIDTH",19:"COMPOSIT_STATE",20:"STRUCT_START",21:"STRUCT_STOP",22:"STATE_DESCR",23:"AS",24:"ID",25:"FORK",26:"JOIN",27:"CHOICE",28:"CONCURRENT",29:"note",31:"NOTE_TEXT",33:"acc_title",34:"acc_title_value",35:"acc_descr",36:"acc_descr_value",37:"acc_descr_multiline_value",38:"classDef",39:"CLASSDEF_ID",40:"CLASSDEF_STYLEOPTS",41:"DEFAULT",42:"style",43:"STYLE_IDS",44:"STYLEDEF_STYLEOPTS",45:"class",46:"CLASSENTITY_IDS",47:"STYLECLASS",48:"direction_tb",49:"direction_bt",50:"direction_rl",51:"direction_lr",53:";",54:"EDGE_STATE",55:"STYLE_SEPARATOR",56:"left_of",57:"right_of"},productions_:[0,[3,2],[3,2],[3,2],[7,0],[7,2],[8,2],[8,1],[8,1],[9,1],[9,1],[9,1],[9,1],[9,2],[9,3],[9,4],[9,1],[9,2],[9,1],[9,4],[9,3],[9,6],[9,1],[9,1],[9,1],[9,1],[9,4],[9,4],[9,1],[9,2],[9,2],[9,1],[10,3],[10,3],[11,3],[12,3],[32,1],[32,1],[32,1],[32,1],[52,1],[52,1],[13,1],[13,1],[13,3],[13,3],[30,1],[30,1]],performAction:i(function(o,u,n,y,b,s,q){var h=s.length-1;switch(b){case 3:return y.setRootDoc(s[h]),s[h];break;case 4:this.$=[];break;case 5:s[h]!="nl"&&(s[h-1].push(s[h]),this.$=s[h-1]);break;case 6:case 7:this.$=s[h];break;case 8:this.$="nl";break;case 12:this.$=s[h];break;case 13:let ht=s[h-1];ht.description=y.trimColon(s[h]),this.$=ht;break;case 14:this.$={stmt:"relation",state1:s[h-2],state2:s[h]};break;case 15:let dt=y.trimColon(s[h]);this.$={stmt:"relation",state1:s[h-3],state2:s[h-1],description:dt};break;case 19:this.$={stmt:"state",id:s[h-3],type:"default",description:"",doc:s[h-1]};break;case 20:var U=s[h],W=s[h-2].trim();if(s[h].match(":")){var Q=s[h].split(":");U=Q[0],W=[W,Q[1]]}this.$={stmt:"state",id:U,type:"default",description:W};break;case 21:this.$={stmt:"state",id:s[h-3],type:"default",description:s[h-5],doc:s[h-1]};break;case 22:this.$={stmt:"state",id:s[h],type:"fork"};break;case 23:this.$={stmt:"state",id:s[h],type:"join"};break;case 24:this.$={stmt:"state",id:s[h],type:"choice"};break;case 25:this.$={stmt:"state",id:y.getDividerId(),type:"divider"};break;case 26:this.$={stmt:"state",id:s[h-1].trim(),note:{position:s[h-2].trim(),text:s[h].trim()}};break;case 29:this.$=s[h].trim(),y.setAccTitle(this.$);break;case 30:case 31:this.$=s[h].trim(),y.setAccDescription(this.$);break;case 32:case 33:this.$={stmt:"classDef",id:s[h-1].trim(),classes:s[h].trim()};break;case 34:this.$={stmt:"style",id:s[h-1].trim(),styleClass:s[h].trim()};break;case 35:this.$={stmt:"applyClass",id:s[h-1].trim(),styleClass:s[h].trim()};break;case 36:y.setDirection("TB"),this.$={stmt:"dir",value:"TB"};break;case 37:y.setDirection("BT"),this.$={stmt:"dir",value:"BT"};break;case 38:y.setDirection("RL"),this.$={stmt:"dir",value:"RL"};break;case 39:y.setDirection("LR"),this.$={stmt:"dir",value:"LR"};break;case 42:case 43:this.$={stmt:"state",id:s[h].trim(),type:"default",description:""};break;case 44:this.$={stmt:"state",id:s[h-2].trim(),classes:[s[h].trim()],type:"default",description:""};break;case 45:this.$={stmt:"state",id:s[h-2].trim(),classes:[s[h].trim()],type:"default",description:""};break}},"anonymous"),table:[{3:1,4:e,5:c,6:a},{1:[3]},{3:5,4:e,5:c,6:a},{3:6,4:e,5:c,6:a},t([1,4,5,16,17,19,22,24,25,26,27,28,29,33,35,37,38,42,45,48,49,50,51,54],d,{7:7}),{1:[2,1]},{1:[2,2]},{1:[2,3],4:r,5:p,8:8,9:10,10:12,11:13,12:14,13:15,16:g,17:l,19:_,22:T,24:m,25:Y,26:F,27:I,28:f,29:L,32:25,33:R,35:V,37:M,38:N,42:w,45:it,48:at,49:nt,50:lt,51:ot,54:J},t(S,[2,5]),{9:38,10:12,11:13,12:14,13:15,16:g,17:l,19:_,22:T,24:m,25:Y,26:F,27:I,28:f,29:L,32:25,33:R,35:V,37:M,38:N,42:w,45:it,48:at,49:nt,50:lt,51:ot,54:J},t(S,[2,7]),t(S,[2,8]),t(S,[2,9]),t(S,[2,10]),t(S,[2,11]),t(S,[2,12],{14:[1,39],15:[1,40]}),t(S,[2,16]),{18:[1,41]},t(S,[2,18],{20:[1,42]}),{23:[1,43]},t(S,[2,22]),t(S,[2,23]),t(S,[2,24]),t(S,[2,25]),{30:44,31:[1,45],56:[1,46],57:[1,47]},t(S,[2,28]),{34:[1,48]},{36:[1,49]},t(S,[2,31]),{39:[1,50],41:[1,51]},{43:[1,52]},{46:[1,53]},t(ct,[2,42],{55:[1,54]}),t(ct,[2,43],{55:[1,55]}),t(S,[2,36]),t(S,[2,37]),t(S,[2,38]),t(S,[2,39]),t(S,[2,6]),t(S,[2,13]),{13:56,24:m,54:J},t(S,[2,17]),t(Mt,d,{7:57}),{24:[1,58]},{24:[1,59]},{23:[1,60]},{24:[2,46]},{24:[2,47]},t(S,[2,29]),t(S,[2,30]),{40:[1,61]},{40:[1,62]},{44:[1,63]},{47:[1,64]},{24:[1,65]},{24:[1,66]},t(S,[2,14],{14:[1,67]}),{4:r,5:p,8:8,9:10,10:12,11:13,12:14,13:15,16:g,17:l,19:_,21:[1,68],22:T,24:m,25:Y,26:F,27:I,28:f,29:L,32:25,33:R,35:V,37:M,38:N,42:w,45:it,48:at,49:nt,50:lt,51:ot,54:J},t(S,[2,20],{20:[1,69]}),{31:[1,70]},{24:[1,71]},t(S,[2,32]),t(S,[2,33]),t(S,[2,34]),t(S,[2,35]),t(ct,[2,44]),t(ct,[2,45]),t(S,[2,15]),t(S,[2,19]),t(Mt,d,{7:72}),t(S,[2,26]),t(S,[2,27]),{4:r,5:p,8:8,9:10,10:12,11:13,12:14,13:15,16:g,17:l,19:_,21:[1,73],22:T,24:m,25:Y,26:F,27:I,28:f,29:L,32:25,33:R,35:V,37:M,38:N,42:w,45:it,48:at,49:nt,50:lt,51:ot,54:J},t(S,[2,21])],defaultActions:{5:[2,1],6:[2,2],46:[2,46],47:[2,47]},parseError:i(function(o,u){if(u.recoverable)this.trace(o);else{var n=new Error(o);throw n.hash=u,n}},"parseError"),parse:i(function(o){var u=this,n=[0],y=[],b=[null],s=[],q=this.table,h="",U=0,W=0,Q=0,ht=2,dt=1,Pe=s.slice.call(arguments,1),E=Object.create(this.lexer),j={yy:{}};for(var Ct in this.yy)Object.prototype.hasOwnProperty.call(this.yy,Ct)&&(j.yy[Ct]=this.yy[Ct]);E.setInput(o,j.yy),j.yy.lexer=E,j.yy.parser=this,typeof E.yylloc=="undefined"&&(E.yylloc={});var xt=E.yylloc;s.push(xt);var Ge=E.options&&E.options.ranges;typeof j.yy.parseError=="function"?this.parseError=j.yy.parseError:this.parseError=Object.getPrototypeOf(this).parseError;function Be(C){n.length=n.length-2*C,b.length=b.length-C,s.length=s.length-C}i(Be,"popStack");function Ut(){var C;return C=y.pop()||E.lex()||dt,typeof C!="number"&&(C instanceof Array&&(y=C,C=y.pop()),C=u.symbols_[C]||C),C}i(Ut,"lex");for(var v,At,H,A,As,Lt,X={},ft,O,jt,pt;;){if(H=n[n.length-1],this.defaultActions[H]?A=this.defaultActions[H]:((v===null||typeof v=="undefined")&&(v=Ut()),A=q[H]&&q[H][v]),typeof A=="undefined"||!A.length||!A[0]){var It="";pt=[];for(ft in q[H])this.terminals_[ft]&&ft>ht&&pt.push("'"+this.terminals_[ft]+"'");E.showPosition?It="Parse error on line "+(U+1)+`:
`+E.showPosition()+`
Expecting `+pt.join(", ")+", got '"+(this.terminals_[v]||v)+"'":It="Parse error on line "+(U+1)+": Unexpected "+(v==dt?"end of input":"'"+(this.terminals_[v]||v)+"'"),this.parseError(It,{text:E.match,token:this.terminals_[v]||v,line:E.yylineno,loc:xt,expected:pt})}if(A[0]instanceof Array&&A.length>1)throw new Error("Parse Error: multiple actions possible at state: "+H+", token: "+v);switch(A[0]){case 1:n.push(v),b.push(E.yytext),s.push(E.yylloc),n.push(A[1]),v=null,At?(v=At,At=null):(W=E.yyleng,h=E.yytext,U=E.yylineno,xt=E.yylloc,Q>0&&Q--);break;case 2:if(O=this.productions_[A[1]][1],X.$=b[b.length-O],X._$={first_line:s[s.length-(O||1)].first_line,last_line:s[s.length-1].last_line,first_column:s[s.length-(O||1)].first_column,last_column:s[s.length-1].last_column},Ge&&(X._$.range=[s[s.length-(O||1)].range[0],s[s.length-1].range[1]]),Lt=this.performAction.apply(X,[h,W,U,j.yy,A[1],b,s].concat(Pe)),typeof Lt!="undefined")return Lt;O&&(n=n.slice(0,-1*O*2),b=b.slice(0,-1*O),s=s.slice(0,-1*O)),n.push(this.productions_[A[1]][0]),b.push(X.$),s.push(X._$),jt=q[n[n.length-2]][n[n.length-1]],n.push(jt);break;case 3:return!0}}return!0},"parse")},$e=function(){var $={EOF:1,parseError:i(function(u,n){if(this.yy.parser)this.yy.parser.parseError(u,n);else throw new Error(u)},"parseError"),setInput:i(function(o,u){return this.yy=u||this.yy||{},this._input=o,this._more=this._backtrack=this.done=!1,this.yylineno=this.yyleng=0,this.yytext=this.matched=this.match="",this.conditionStack=["INITIAL"],this.yylloc={first_line:1,first_column:0,last_line:1,last_column:0},this.options.ranges&&(this.yylloc.range=[0,0]),this.offset=0,this},"setInput"),input:i(function(){var o=this._input[0];this.yytext+=o,this.yyleng++,this.offset++,this.match+=o,this.matched+=o;var u=o.match(/(?:\r\n?|\n).*/g);return u?(this.yylineno++,this.yylloc.last_line++):this.yylloc.last_column++,this.options.ranges&&this.yylloc.range[1]++,this._input=this._input.slice(1),o},"input"),unput:i(function(o){var u=o.length,n=o.split(/(?:\r\n?|\n)/g);this._input=o+this._input,this.yytext=this.yytext.substr(0,this.yytext.length-u),this.offset-=u;var y=this.match.split(/(?:\r\n?|\n)/g);this.match=this.match.substr(0,this.match.length-1),this.matched=this.matched.substr(0,this.matched.length-1),n.length-1&&(this.yylineno-=n.length-1);var b=this.yylloc.range;return this.yylloc={first_line:this.yylloc.first_line,last_line:this.yylineno+1,first_column:this.yylloc.first_column,last_column:n?(n.length===y.length?this.yylloc.first_column:0)+y[y.length-n.length].length-n[0].length:this.yylloc.first_column-u},this.options.ranges&&(this.yylloc.range=[b[0],b[0]+this.yyleng-u]),this.yyleng=this.yytext.length,this},"unput"),more:i(function(){return this._more=!0,this},"more"),reject:i(function(){if(this.options.backtrack_lexer)this._backtrack=!0;else return this.parseError("Lexical error on line "+(this.yylineno+1)+`. You can only invoke reject() in the lexer when the lexer is of the backtracking persuasion (options.backtrack_lexer = true).
`+this.showPosition(),{text:"",token:null,line:this.yylineno});return this},"reject"),less:i(function(o){this.unput(this.match.slice(o))},"less"),pastInput:i(function(){var o=this.matched.substr(0,this.matched.length-this.match.length);return(o.length>20?"...":"")+o.substr(-20).replace(/\n/g,"")},"pastInput"),upcomingInput:i(function(){var o=this.match;return o.length<20&&(o+=this._input.substr(0,20-o.length)),(o.substr(0,20)+(o.length>20?"...":"")).replace(/\n/g,"")},"upcomingInput"),showPosition:i(function(){var o=this.pastInput(),u=new Array(o.length+1).join("-");return o+this.upcomingInput()+`
`+u+"^"},"showPosition"),test_match:i(function(o,u){var n,y,b;if(this.options.backtrack_lexer&&(b={yylineno:this.yylineno,yylloc:{first_line:this.yylloc.first_line,last_line:this.last_line,first_column:this.yylloc.first_column,last_column:this.yylloc.last_column},yytext:this.yytext,match:this.match,matches:this.matches,matched:this.matched,yyleng:this.yyleng,offset:this.offset,_more:this._more,_input:this._input,yy:this.yy,conditionStack:this.conditionStack.slice(0),done:this.done},this.options.ranges&&(b.yylloc.range=this.yylloc.range.slice(0))),y=o[0].match(/(?:\r\n?|\n).*/g),y&&(this.yylineno+=y.length),this.yylloc={first_line:this.yylloc.last_line,last_line:this.yylineno+1,first_column:this.yylloc.last_column,last_column:y?y[y.length-1].length-y[y.length-1].match(/\r?\n?/)[0].length:this.yylloc.last_column+o[0].length},this.yytext+=o[0],this.match+=o[0],this.matches=o,this.yyleng=this.yytext.length,this.options.ranges&&(this.yylloc.range=[this.offset,this.offset+=this.yyleng]),this._more=!1,this._backtrack=!1,this._input=this._input.slice(o[0].length),this.matched+=o[0],n=this.performAction.call(this,this.yy,this,u,this.conditionStack[this.conditionStack.length-1]),this.done&&this._input&&(this.done=!1),n)return n;if(this._backtrack){for(var s in b)this[s]=b[s];return!1}return!1},"test_match"),next:i(function(){if(this.done)return this.EOF;this._input||(this.done=!0);var o,u,n,y;this._more||(this.yytext="",this.match="");for(var b=this._currentRules(),s=0;s<b.length;s++)if(n=this._input.match(this.rules[b[s]]),n&&(!u||n[0].length>u[0].length)){if(u=n,y=s,this.options.backtrack_lexer){if(o=this.test_match(n,b[s]),o!==!1)return o;if(this._backtrack){u=!1;continue}else return!1}else if(!this.options.flex)break}return u?(o=this.test_match(u,b[y]),o!==!1?o:!1):this._input===""?this.EOF:this.parseError("Lexical error on line "+(this.yylineno+1)+`. Unrecognized text.
`+this.showPosition(),{text:"",token:null,line:this.yylineno})},"next"),lex:i(function(){var u=this.next();return u||this.lex()},"lex"),begin:i(function(u){this.conditionStack.push(u)},"begin"),popState:i(function(){var u=this.conditionStack.length-1;return u>0?this.conditionStack.pop():this.conditionStack[0]},"popState"),_currentRules:i(function(){return this.conditionStack.length&&this.conditionStack[this.conditionStack.length-1]?this.conditions[this.conditionStack[this.conditionStack.length-1]].rules:this.conditions.INITIAL.rules},"_currentRules"),topState:i(function(u){return u=this.conditionStack.length-1-Math.abs(u||0),u>=0?this.conditionStack[u]:"INITIAL"},"topState"),pushState:i(function(u){this.begin(u)},"pushState"),stateStackSize:i(function(){return this.conditionStack.length},"stateStackSize"),options:{"case-insensitive":!0},performAction:i(function(u,n,y,b){var s=b;switch(y){case 0:return 41;case 1:return 48;case 2:return 49;case 3:return 50;case 4:return 51;case 5:break;case 6:break;case 7:return 5;case 8:break;case 9:break;case 10:break;case 11:break;case 12:return this.pushState("SCALE"),17;break;case 13:return 18;case 14:this.popState();break;case 15:return this.begin("acc_title"),33;break;case 16:return this.popState(),"acc_title_value";break;case 17:return this.begin("acc_descr"),35;break;case 18:return this.popState(),"acc_descr_value";break;case 19:this.begin("acc_descr_multiline");break;case 20:this.popState();break;case 21:return"acc_descr_multiline_value";case 22:return this.pushState("CLASSDEF"),38;break;case 23:return this.popState(),this.pushState("CLASSDEFID"),"DEFAULT_CLASSDEF_ID";break;case 24:return this.popState(),this.pushState("CLASSDEFID"),39;break;case 25:return this.popState(),40;break;case 26:return this.pushState("CLASS"),45;break;case 27:return this.popState(),this.pushState("CLASS_STYLE"),46;break;case 28:return this.popState(),47;break;case 29:return this.pushState("STYLE"),42;break;case 30:return this.popState(),this.pushState("STYLEDEF_STYLES"),43;break;case 31:return this.popState(),44;break;case 32:return this.pushState("SCALE"),17;break;case 33:return 18;case 34:this.popState();break;case 35:this.pushState("STATE");break;case 36:return this.popState(),n.yytext=n.yytext.slice(0,-8).trim(),25;break;case 37:return this.popState(),n.yytext=n.yytext.slice(0,-8).trim(),26;break;case 38:return this.popState(),n.yytext=n.yytext.slice(0,-10).trim(),27;break;case 39:return this.popState(),n.yytext=n.yytext.slice(0,-8).trim(),25;break;case 40:return this.popState(),n.yytext=n.yytext.slice(0,-8).trim(),26;break;case 41:return this.popState(),n.yytext=n.yytext.slice(0,-10).trim(),27;break;case 42:return 48;case 43:return 49;case 44:return 50;case 45:return 51;case 46:this.pushState("STATE_STRING");break;case 47:return this.pushState("STATE_ID"),"AS";break;case 48:return this.popState(),"ID";break;case 49:this.popState();break;case 50:return"STATE_DESCR";case 51:return 19;case 52:this.popState();break;case 53:return this.popState(),this.pushState("struct"),20;break;case 54:break;case 55:return this.popState(),21;break;case 56:break;case 57:return this.begin("NOTE"),29;break;case 58:return this.popState(),this.pushState("NOTE_ID"),56;break;case 59:return this.popState(),this.pushState("NOTE_ID"),57;break;case 60:this.popState(),this.pushState("FLOATING_NOTE");break;case 61:return this.popState(),this.pushState("FLOATING_NOTE_ID"),"AS";break;case 62:break;case 63:return"NOTE_TEXT";case 64:return this.popState(),"ID";break;case 65:return this.popState(),this.pushState("NOTE_TEXT"),24;break;case 66:return this.popState(),n.yytext=n.yytext.substr(2).trim(),31;break;case 67:return this.popState(),n.yytext=n.yytext.slice(0,-8).trim(),31;break;case 68:return 6;case 69:return 6;case 70:return 16;case 71:return 54;case 72:return 24;case 73:return n.yytext=n.yytext.trim(),14;break;case 74:return 15;case 75:return 28;case 76:return 55;case 77:return 5;case 78:return"INVALID"}},"anonymous"),rules:[/^(?:default\b)/i,/^(?:.*direction\s+TB[^\n]*)/i,/^(?:.*direction\s+BT[^\n]*)/i,/^(?:.*direction\s+RL[^\n]*)/i,/^(?:.*direction\s+LR[^\n]*)/i,/^(?:%%(?!\{)[^\n]*)/i,/^(?:[^\}]%%[^\n]*)/i,/^(?:[\n]+)/i,/^(?:[\s]+)/i,/^(?:((?!\n)\s)+)/i,/^(?:#[^\n]*)/i,/^(?:%[^\n]*)/i,/^(?:scale\s+)/i,/^(?:\d+)/i,/^(?:\s+width\b)/i,/^(?:accTitle\s*:\s*)/i,/^(?:(?!\n||)*[^\n]*)/i,/^(?:accDescr\s*:\s*)/i,/^(?:(?!\n||)*[^\n]*)/i,/^(?:accDescr\s*\{\s*)/i,/^(?:[\}])/i,/^(?:[^\}]*)/i,/^(?:classDef\s+)/i,/^(?:DEFAULT\s+)/i,/^(?:\w+\s+)/i,/^(?:[^\n]*)/i,/^(?:class\s+)/i,/^(?:(\w+)+((,\s*\w+)*))/i,/^(?:[^\n]*)/i,/^(?:style\s+)/i,/^(?:[\w,]+\s+)/i,/^(?:[^\n]*)/i,/^(?:scale\s+)/i,/^(?:\d+)/i,/^(?:\s+width\b)/i,/^(?:state\s+)/i,/^(?:.*<<fork>>)/i,/^(?:.*<<join>>)/i,/^(?:.*<<choice>>)/i,/^(?:.*\[\[fork\]\])/i,/^(?:.*\[\[join\]\])/i,/^(?:.*\[\[choice\]\])/i,/^(?:.*direction\s+TB[^\n]*)/i,/^(?:.*direction\s+BT[^\n]*)/i,/^(?:.*direction\s+RL[^\n]*)/i,/^(?:.*direction\s+LR[^\n]*)/i,/^(?:["])/i,/^(?:\s*as\s+)/i,/^(?:[^\n\{]*)/i,/^(?:["])/i,/^(?:[^"]*)/i,/^(?:[^\n\s\{]+)/i,/^(?:\n)/i,/^(?:\{)/i,/^(?:%%(?!\{)[^\n]*)/i,/^(?:\})/i,/^(?:[\n])/i,/^(?:note\s+)/i,/^(?:left of\b)/i,/^(?:right of\b)/i,/^(?:")/i,/^(?:\s*as\s*)/i,/^(?:["])/i,/^(?:[^"]*)/i,/^(?:[^\n]*)/i,/^(?:\s*[^:\n\s\-]+)/i,/^(?:\s*:[^:\n;]+)/i,/^(?:[\s\S]*?end note\b)/i,/^(?:stateDiagram\s+)/i,/^(?:stateDiagram-v2\s+)/i,/^(?:hide empty description\b)/i,/^(?:\[\*\])/i,/^(?:[^:\n\s\-\{]+)/i,/^(?:\s*:[^:\n;]+)/i,/^(?:-->)/i,/^(?:--)/i,/^(?::::)/i,/^(?:$)/i,/^(?:.)/i],conditions:{LINE:{rules:[9,10],inclusive:!1},struct:{rules:[9,10,22,26,29,35,42,43,44,45,54,55,56,57,71,72,73,74,75],inclusive:!1},FLOATING_NOTE_ID:{rules:[64],inclusive:!1},FLOATING_NOTE:{rules:[61,62,63],inclusive:!1},NOTE_TEXT:{rules:[66,67],inclusive:!1},NOTE_ID:{rules:[65],inclusive:!1},NOTE:{rules:[58,59,60],inclusive:!1},STYLEDEF_STYLEOPTS:{rules:[],inclusive:!1},STYLEDEF_STYLES:{rules:[31],inclusive:!1},STYLE_IDS:{rules:[],inclusive:!1},STYLE:{rules:[30],inclusive:!1},CLASS_STYLE:{rules:[28],inclusive:!1},CLASS:{rules:[27],inclusive:!1},CLASSDEFID:{rules:[25],inclusive:!1},CLASSDEF:{rules:[23,24],inclusive:!1},acc_descr_multiline:{rules:[20,21],inclusive:!1},acc_descr:{rules:[18],inclusive:!1},acc_title:{rules:[16],inclusive:!1},SCALE:{rules:[13,14,33,34],inclusive:!1},ALIAS:{rules:[],inclusive:!1},STATE_ID:{rules:[48],inclusive:!1},STATE_STRING:{rules:[49,50],inclusive:!1},FORK_STATE:{rules:[],inclusive:!1},STATE:{rules:[9,10,36,37,38,39,40,41,46,47,51,52,53],inclusive:!1},ID:{rules:[9,10],inclusive:!1},INITIAL:{rules:[0,1,2,3,4,5,6,7,8,10,11,12,15,17,19,22,26,29,32,35,53,57,68,69,70,71,72,73,74,76,77,78],inclusive:!0}}};return $}();Dt.lexer=$e;function ut(){this.yy={}}return i(ut,"Parser"),ut.prototype=Dt,Dt.Parser=ut,new ut}();Nt.parser=Nt;var Ns=Nt,Ye="LR",ue="TB",_t="state",$t="relation",Fe="classDef",Ve="style",Me="applyClass",st="default",he="divider",de="fill:none",fe="fill: #333",pe="c",Se="text",ye="normal",Rt="rect",Ot="rectWithTitle",Ue="stateStart",je="stateEnd",re="divider",ie="roundedWithTitle",He="note",ze="noteGroup",rt="statediagram",We="state",Xe=`${rt}-${We}`,ge="transition",Ke="note",Je="note-edge",qe=`${ge} ${Je}`,Qe=`${rt}-${Ke}`,Ze="cluster",ts=`${rt}-${Ze}`,es="cluster-alt",ss=`${rt}-${es}`,be="parent",_e="note",rs="state",Pt="----",is=`${Pt}${_e}`,ae=`${Pt}${be}`,Te=i((t,e=ue)=>{if(!t.doc)return e;let c=e;for(let a of t.doc)a.stmt==="dir"&&(c=a.value);return c},"getDir"),as=i(function(t,e){return e.db.extract(e.db.getRootDocV2()),e.db.getClasses()},"getClasses"),ns=i(async function(t,e,c,a){var T,m;k.info("REF0:"),k.info("Drawing state diagram (v2)",e);let{securityLevel:d,state:r,layout:p}=x();a.db.extract(a.db.getRootDocV2());let g=a.db.getData(),l=ee(e,d);g.type=a.type,g.layoutAlgorithm=p,g.nodeSpacing=(r==null?void 0:r.nodeSpacing)||50,g.rankSpacing=(r==null?void 0:r.rankSpacing)||50,g.markers=["barb"],g.diagramId=e,await te(g,l);let _=8;Zt.insertTitle(l,"statediagramTitleText",(T=r==null?void 0:r.titleTopMargin)!=null?T:25,a.db.getDiagramTitle()),se(l,_,rt,(m=r==null?void 0:r.useMaxWidth)!=null?m:!0)},"draw"),ws={getClasses:as,draw:ns,getDir:Te},yt=new Map,G=0;function gt(t="",e=0,c="",a=Pt){let d=c!==null&&c.length>0?`${a}${c}`:"";return`${rs}-${t}${d}-${e}`}i(gt,"stateDomId");var ls=i((t,e,c,a,d,r,p,g)=>{k.trace("items",e),e.forEach(l=>{switch(l.stmt){case _t:tt(t,l,c,a,d,r,p,g);break;case st:tt(t,l,c,a,d,r,p,g);break;case $t:{tt(t,l.state1,c,a,d,r,p,g),tt(t,l.state2,c,a,d,r,p,g);let _={id:"edge"+G,start:l.state1.id,end:l.state2.id,arrowhead:"normal",arrowTypeEnd:"arrow_barb",style:de,labelStyle:"",label:P.sanitizeText(l.description,x()),arrowheadStyle:fe,labelpos:pe,labelType:Se,thickness:ye,classes:ge,look:p};d.push(_),G++}break}})},"setupDoc"),ne=i((t,e=ue)=>{let c=e;if(t.doc)for(let a of t.doc)a.stmt==="dir"&&(c=a.value);return c},"getDir");function Z(t,e,c){if(!e.id||e.id==="</join></fork>"||e.id==="</choice>")return;e.cssClasses&&(Array.isArray(e.cssCompiledStyles)||(e.cssCompiledStyles=[]),e.cssClasses.split(" ").forEach(d=>{if(c.get(d)){let r=c.get(d);e.cssCompiledStyles=[...e.cssCompiledStyles,...r.styles]}}));let a=t.find(d=>d.id===e.id);a?Object.assign(a,e):t.push(e)}i(Z,"insertOrUpdateNode");function Ee(t){var e,c;return(c=(e=t==null?void 0:t.classes)==null?void 0:e.join(" "))!=null?c:""}i(Ee,"getClassesFromDbInfo");function ke(t){var e;return(e=t==null?void 0:t.styles)!=null?e:[]}i(ke,"getStylesFromDbInfo");var tt=i((t,e,c,a,d,r,p,g)=>{var Y,F;let l=e.id,_=c.get(l),T=Ee(_),m=ke(_);if(k.info("dataFetcher parsedItem",e,_,m),l!=="root"){let I=Rt;e.start===!0?I=Ue:e.start===!1&&(I=je),e.type!==st&&(I=e.type),yt.get(l)||yt.set(l,{id:l,shape:I,description:P.sanitizeText(l,x()),cssClasses:`${T} ${Xe}`,cssStyles:m});let f=yt.get(l);e.description&&(Array.isArray(f.description)?(f.shape=Ot,f.description.push(e.description)):((Y=f.description)==null?void 0:Y.length)>0?(f.shape=Ot,f.description===l?f.description=[e.description]:f.description=[f.description,e.description]):(f.shape=Rt,f.description=e.description),f.description=P.sanitizeTextOrArray(f.description,x())),((F=f.description)==null?void 0:F.length)===1&&f.shape===Ot&&(f.type==="group"?f.shape=ie:f.shape=Rt),!f.type&&e.doc&&(k.info("Setting cluster for XCX",l,ne(e)),f.type="group",f.isGroup=!0,f.dir=ne(e),f.shape=e.type===he?re:ie,f.cssClasses=`${f.cssClasses} ${ts} ${r?ss:""}`);let L={labelStyle:"",shape:f.shape,label:f.description,cssClasses:f.cssClasses,cssCompiledStyles:[],cssStyles:f.cssStyles,id:l,dir:f.dir,domId:gt(l,G),type:f.type,isGroup:f.type==="group",padding:8,rx:10,ry:10,look:p};if(L.shape===re&&(L.label=""),t&&t.id!=="root"&&(k.trace("Setting node ",l," to be child of its parent ",t.id),L.parentId=t.id),L.centerLabel=!0,e.note){let R={labelStyle:"",shape:He,label:e.note.text,cssClasses:Qe,cssStyles:[],cssCompilesStyles:[],id:l+is+"-"+G,domId:gt(l,G,_e),type:f.type,isGroup:f.type==="group",padding:x().flowchart.padding,look:p,position:e.note.position},V=l+ae,M={labelStyle:"",shape:ze,label:e.note.text,cssClasses:f.cssClasses,cssStyles:[],id:l+ae,domId:gt(l,G,be),type:"group",isGroup:!0,padding:16,look:p,position:e.note.position};G++,M.id=V,R.parentId=V,Z(a,M,g),Z(a,R,g),Z(a,L,g);let N=l,w=R.id;e.note.position==="left of"&&(N=R.id,w=l),d.push({id:N+"-"+w,start:N,end:w,arrowhead:"none",arrowTypeEnd:"",style:de,labelStyle:"",classes:qe,arrowheadStyle:fe,labelpos:pe,labelType:Se,thickness:ye,look:p})}else Z(a,L,g)}e.doc&&(k.trace("Adding nodes children "),ls(e,e.doc,c,a,d,!r,p,g))},"dataFetcher"),os=i(()=>{yt.clear(),G=0},"reset"),Gt="[*]",ve="start",me=Gt,De="end",le="color",oe="fill",cs="bgFill",us=",";function Bt(){return new Map}i(Bt,"newClassesList");var Tt=[],Yt=[],Ce=Ye,Et=[],K=Bt(),xe=i(()=>({relations:[],states:new Map,documents:{}}),"newDoc"),kt={root:xe()},D=kt.root,et=0,ce=0,hs={LINE:0,DOTTED_LINE:1},ds={AGGREGATION:0,EXTENSION:1,COMPOSITION:2,DEPENDENCY:3},St=i(t=>JSON.parse(JSON.stringify(t)),"clone"),fs=i(t=>{k.info("Setting root doc",t),Et=t},"setRootDoc"),ps=i(()=>Et,"getRootDoc"),bt=i((t,e,c)=>{if(e.stmt===$t)bt(t,e.state1,!0),bt(t,e.state2,!1);else if(e.stmt===_t&&(e.id==="[*]"?(e.id=c?t.id+"_start":t.id+"_end",e.start=c):e.id=e.id.trim()),e.doc){let a=[],d=[],r;for(r=0;r<e.doc.length;r++)if(e.doc[r].type===he){let p=St(e.doc[r]);p.doc=St(d),a.push(p),d=[]}else d.push(e.doc[r]);if(a.length>0&&d.length>0){let p={stmt:_t,id:Qt(),type:"divider",doc:St(d)};a.push(St(p)),e.doc=a}e.doc.forEach(p=>bt(e,p,!0))}},"docTranslator"),Ft=i(()=>(bt({id:"root"},{id:"root",doc:Et},!0),{id:"root",doc:Et}),"getRootDocV2"),Ss=i(t=>{let e;t.doc?e=t.doc:e=t,k.info(e),Ae(!0),k.info("Extract initial document:",e),e.forEach(r=>{switch(k.warn("Statement",r.stmt),r.stmt){case _t:B(r.id.trim(),r.type,r.doc,r.description,r.note,r.classes,r.styles,r.textStyles);break;case $t:Ne(r.state1,r.state2,r.description);break;case Fe:we(r.id.trim(),r.classes);break;case Ve:{let p=r.id.trim().split(","),g=r.styleClass.split(",");p.forEach(l=>{let _=z(l);if(_===void 0){let T=l.trim();B(T),_=z(T)}_.styles=g.map(T=>{var m;return(m=T.replace(/;/g,""))==null?void 0:m.trim()})})}break;case Me:Vt(r.id.trim(),r.styleClass);break}});let c=Le(),d=x().look;os(),tt(void 0,Ft(),c,Tt,Yt,!0,d,K),Tt.forEach(r=>{if(Array.isArray(r.label)){if(r.description=r.label.slice(1),r.isGroup&&r.description.length>0)throw new Error("Group nodes can only have label. Remove the additional description for node ["+r.id+"]");r.label=r.label[0]}})},"extract"),B=i(function(t,e=st,c=null,a=null,d=null,r=null,p=null,g=null){let l=t==null?void 0:t.trim();if(D.states.has(l)?(D.states.get(l).doc||(D.states.get(l).doc=c),D.states.get(l).type||(D.states.get(l).type=e)):(k.info("Adding state ",l,a),D.states.set(l,{id:l,descriptions:[],type:e,doc:c,note:d,classes:[],styles:[],textStyles:[]})),a&&(k.info("Setting state description",l,a),typeof a=="string"&&wt(l,a.trim()),typeof a=="object"&&a.forEach(_=>wt(l,_.trim()))),d){let _=D.states.get(l);_.note=d,_.note.text=P.sanitizeText(_.note.text,x())}r&&(k.info("Setting state classes",l,r),(typeof r=="string"?[r]:r).forEach(T=>Vt(l,T.trim()))),p&&(k.info("Setting state styles",l,p),(typeof p=="string"?[p]:p).forEach(T=>Es(l,T.trim()))),g&&(k.info("Setting state styles",l,p),(typeof g=="string"?[g]:g).forEach(T=>ks(l,T.trim())))},"addState"),Ae=i(function(t){Tt=[],Yt=[],kt={root:xe()},D=kt.root,et=0,K=Bt(),t||Ht()},"clear"),z=i(function(t){return D.states.get(t)},"getState"),Le=i(function(){return D.states},"getStates"),ys=i(function(){k.info("Documents = ",kt)},"logDocuments"),gs=i(function(){return D.relations},"getRelations");function vt(t=""){let e=t;return t===Gt&&(et++,e=`${ve}${et}`),e}i(vt,"startIdIfNeeded");function mt(t="",e=st){return t===Gt?ve:e}i(mt,"startTypeIfNeeded");function Ie(t=""){let e=t;return t===me&&(et++,e=`${De}${et}`),e}i(Ie,"endIdIfNeeded");function Re(t="",e=st){return t===me?De:e}i(Re,"endTypeIfNeeded");function Oe(t,e,c){let a=vt(t.id.trim()),d=mt(t.id.trim(),t.type),r=vt(e.id.trim()),p=mt(e.id.trim(),e.type);B(a,d,t.doc,t.description,t.note,t.classes,t.styles,t.textStyles),B(r,p,e.doc,e.description,e.note,e.classes,e.styles,e.textStyles),D.relations.push({id1:a,id2:r,relationTitle:P.sanitizeText(c,x())})}i(Oe,"addRelationObjs");var Ne=i(function(t,e,c){if(typeof t=="object")Oe(t,e,c);else{let a=vt(t.trim()),d=mt(t),r=Ie(e.trim()),p=Re(e);B(a,d),B(r,p),D.relations.push({id1:a,id2:r,title:P.sanitizeText(c,x())})}},"addRelation"),wt=i(function(t,e){let c=D.states.get(t),a=e.startsWith(":")?e.replace(":","").trim():e;c.descriptions.push(P.sanitizeText(a,x()))},"addDescription"),bs=i(function(t){return t.substring(0,1)===":"?t.substr(2).trim():t.trim()},"cleanupLabel"),_s=i(()=>(ce++,"divider-id-"+ce),"getDividerId"),we=i(function(t,e=""){K.has(t)||K.set(t,{id:t,styles:[],textStyles:[]});let c=K.get(t);e!=null&&e.split(us).forEach(a=>{let d=a.replace(/([^;]*);/,"$1").trim();if(RegExp(le).exec(a)){let p=d.replace(oe,cs).replace(le,oe);c.textStyles.push(p)}c.styles.push(d)})},"addStyleClass"),Ts=i(function(){return K},"getClasses"),Vt=i(function(t,e){t.split(",").forEach(function(c){let a=z(c);if(a===void 0){let d=c.trim();B(d),a=z(d)}a.classes.push(e)})},"setCssClass"),Es=i(function(t,e){let c=z(t);c!==void 0&&c.styles.push(e)},"setStyle"),ks=i(function(t,e){let c=z(t);c!==void 0&&c.textStyles.push(e)},"setTextStyle"),vs=i(()=>Ce,"getDirection"),ms=i(t=>{Ce=t},"setDirection"),Ds=i(t=>t&&t[0]===":"?t.substr(1).trim():t.trim(),"trimColon"),Cs=i(()=>{let t=x();return{nodes:Tt,edges:Yt,other:{},config:t,direction:Te(Ft())}},"getData"),$s={getConfig:i(()=>x().state,"getConfig"),getData:Cs,addState:B,clear:Ae,getState:z,getStates:Le,getRelations:gs,getClasses:Ts,getDirection:vs,addRelation:Ne,getDividerId:_s,setDirection:ms,cleanupLabel:bs,lineType:hs,relationType:ds,logDocuments:ys,getRootDoc:ps,setRootDoc:fs,getRootDocV2:Ft,extract:Ss,trimColon:Ds,getAccTitle:Wt,setAccTitle:zt,getAccDescription:Kt,setAccDescription:Xt,addStyleClass:we,setCssClass:Vt,addDescription:wt,setDiagramTitle:Jt,getDiagramTitle:qt},xs=i(t=>`
defs #statediagram-barbEnd {
    fill: ${t.transitionColor};
    stroke: ${t.transitionColor};
  }
g.stateGroup text {
  fill: ${t.nodeBorder};
  stroke: none;
  font-size: 10px;
}
g.stateGroup text {
  fill: ${t.textColor};
  stroke: none;
  font-size: 10px;

}
g.stateGroup .state-title {
  font-weight: bolder;
  fill: ${t.stateLabelColor};
}

g.stateGroup rect {
  fill: ${t.mainBkg};
  stroke: ${t.nodeBorder};
}

g.stateGroup line {
  stroke: ${t.lineColor};
  stroke-width: 1;
}

.transition {
  stroke: ${t.transitionColor};
  stroke-width: 1;
  fill: none;
}

.stateGroup .composit {
  fill: ${t.background};
  border-bottom: 1px
}

.stateGroup .alt-composit {
  fill: #e0e0e0;
  border-bottom: 1px
}

.state-note {
  stroke: ${t.noteBorderColor};
  fill: ${t.noteBkgColor};

  text {
    fill: ${t.noteTextColor};
    stroke: none;
    font-size: 10px;
  }
}

.stateLabel .box {
  stroke: none;
  stroke-width: 0;
  fill: ${t.mainBkg};
  opacity: 0.5;
}

.edgeLabel .label rect {
  fill: ${t.labelBackgroundColor};
  opacity: 0.5;
}
.edgeLabel {
  background-color: ${t.edgeLabelBackground};
  p {
    background-color: ${t.edgeLabelBackground};
  }
  rect {
    opacity: 0.5;
    background-color: ${t.edgeLabelBackground};
    fill: ${t.edgeLabelBackground};
  }
  text-align: center;
}
.edgeLabel .label text {
  fill: ${t.transitionLabelColor||t.tertiaryTextColor};
}
.label div .edgeLabel {
  color: ${t.transitionLabelColor||t.tertiaryTextColor};
}

.stateLabel text {
  fill: ${t.stateLabelColor};
  font-size: 10px;
  font-weight: bold;
}

.node circle.state-start {
  fill: ${t.specialStateColor};
  stroke: ${t.specialStateColor};
}

.node .fork-join {
  fill: ${t.specialStateColor};
  stroke: ${t.specialStateColor};
}

.node circle.state-end {
  fill: ${t.innerEndBackground};
  stroke: ${t.background};
  stroke-width: 1.5
}
.end-state-inner {
  fill: ${t.compositeBackground||t.background};
  // stroke: ${t.background};
  stroke-width: 1.5
}

.node rect {
  fill: ${t.stateBkg||t.mainBkg};
  stroke: ${t.stateBorder||t.nodeBorder};
  stroke-width: 1px;
}
.node polygon {
  fill: ${t.mainBkg};
  stroke: ${t.stateBorder||t.nodeBorder};;
  stroke-width: 1px;
}
#statediagram-barbEnd {
  fill: ${t.lineColor};
}

.statediagram-cluster rect {
  fill: ${t.compositeTitleBackground};
  stroke: ${t.stateBorder||t.nodeBorder};
  stroke-width: 1px;
}

.cluster-label, .nodeLabel {
  color: ${t.stateLabelColor};
  // line-height: 1;
}

.statediagram-cluster rect.outer {
  rx: 5px;
  ry: 5px;
}
.statediagram-state .divider {
  stroke: ${t.stateBorder||t.nodeBorder};
}

.statediagram-state .title-state {
  rx: 5px;
  ry: 5px;
}
.statediagram-cluster.statediagram-cluster .inner {
  fill: ${t.compositeBackground||t.background};
}
.statediagram-cluster.statediagram-cluster-alt .inner {
  fill: ${t.altBackground?t.altBackground:"#efefef"};
}

.statediagram-cluster .inner {
  rx:0;
  ry:0;
}

.statediagram-state rect.basic {
  rx: 5px;
  ry: 5px;
}
.statediagram-state rect.divider {
  stroke-dasharray: 10,10;
  fill: ${t.altBackground?t.altBackground:"#efefef"};
}

.note-edge {
  stroke-dasharray: 5;
}

.statediagram-note rect {
  fill: ${t.noteBkgColor};
  stroke: ${t.noteBorderColor};
  stroke-width: 1px;
  rx: 0;
  ry: 0;
}
.statediagram-note rect {
  fill: ${t.noteBkgColor};
  stroke: ${t.noteBorderColor};
  stroke-width: 1px;
  rx: 0;
  ry: 0;
}

.statediagram-note text {
  fill: ${t.noteTextColor};
}

.statediagram-note .nodeLabel {
  color: ${t.noteTextColor};
}
.statediagram .edgeLabel {
  color: red; // ${t.noteTextColor};
}

#dependencyStart, #dependencyEnd {
  fill: ${t.lineColor};
  stroke: ${t.lineColor};
  stroke-width: 1;
}

.statediagramTitleText {
  text-anchor: middle;
  font-size: 18px;
  fill: ${t.textColor};
}
`,"getStyles"),Ps=xs;export{Ns as a,ws as b,$s as c,Ps as d};
