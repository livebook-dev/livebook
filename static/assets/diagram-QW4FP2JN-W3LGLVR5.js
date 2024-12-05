import{a as A}from"./chunk-MF5TUIBL.js";import{a as D}from"./chunk-5XJO6U7A.js";import{a as E}from"./chunk-E4RBDSW5.js";import"./chunk-NKCQAART.js";import"./chunk-WNVR7S66.js";import"./chunk-ZP5QXAVO.js";import{l as x}from"./chunk-47P5NBBB.js";import"./chunk-XHGORZV2.js";import{D as y,O as w,S as B,T as S,U as F,V as z,W as P,X as W,Y as T,h as n,j as v,v as $}from"./chunk-KRX7QNR4.js";import"./chunk-UY4K4Z6W.js";import"./chunk-TZMIYTTY.js";import"./chunk-SA2EGKJT.js";import"./chunk-WYMAA4MH.js";import"./chunk-SISR4MA5.js";import"./chunk-24JW6VB3.js";import{a as m}from"./chunk-MGYUK2XN.js";var _={packet:[]},C=structuredClone(_),Y=$.packet,I=n(()=>{let t=x(m(m({},Y),y().packet));return t.showBits&&(t.paddingY+=10),t},"getConfig"),M=n(()=>C.packet,"getPacket"),O=n(t=>{t.length>0&&C.packet.push(t)},"pushWord"),G=n(()=>{B(),C=structuredClone(_)},"clear"),h={pushWord:O,getPacket:M,getConfig:I,clear:G,setAccTitle:S,getAccTitle:F,setDiagramTitle:W,getDiagramTitle:T,getAccDescription:P,setAccDescription:z},H=1e4,K=n(t=>{A(t,h);let e=-1,o=[],i=1,{bitsPerRow:s}=h.getConfig();for(let{start:r,end:a,label:p}of t.blocks){if(a&&a<r)throw new Error(`Packet block ${r} - ${a} is invalid. End must be greater than start.`);if(r!==e+1)throw new Error(`Packet block ${r} - ${a!=null?a:r} is not contiguous. It should start from ${e+1}.`);for(e=a!=null?a:r,v.debug(`Packet block ${r} - ${e} with label ${p}`);o.length<=s+1&&h.getPacket().length<H;){let[b,c]=R({start:r,end:a,label:p},i,s);if(o.push(b),b.end+1===i*s&&(h.pushWord(o),o=[],i++),!c)break;({start:r,end:a,label:p}=c)}}h.pushWord(o)},"populate"),R=n((t,e,o)=>{if(t.end===void 0&&(t.end=t.start),t.start>t.end)throw new Error(`Block start ${t.start} is greater than block end ${t.end}.`);return t.end+1<=e*o?[t,void 0]:[{start:t.start,end:e*o-1,label:t.label},{start:e*o,end:t.end,label:t.label}]},"getNextFittingBlock"),U={parse:n(async t=>{let e=await E("packet",t);v.debug(e),K(e)},"parse")},X=n((t,e,o,i)=>{let s=i.db,r=s.getConfig(),{rowHeight:a,paddingY:p,bitWidth:b,bitsPerRow:c}=r,u=s.getPacket(),l=s.getDiagramTitle(),g=a+p,d=g*(u.length+1)-(l?0:a),k=b*c+2,f=D(e);f.attr("viewbox",`0 0 ${k} ${d}`),w(f,d,k,r.useMaxWidth);for(let[N,L]of u.entries())j(f,L,N,r);f.append("text").text(l).attr("x",k/2).attr("y",d-g/2).attr("dominant-baseline","middle").attr("text-anchor","middle").attr("class","packetTitle")},"draw"),j=n((t,e,o,{rowHeight:i,paddingX:s,paddingY:r,bitWidth:a,bitsPerRow:p,showBits:b})=>{let c=t.append("g"),u=o*(i+r)+r;for(let l of e){let g=l.start%p*a+1,d=(l.end-l.start+1)*a-s;if(c.append("rect").attr("x",g).attr("y",u).attr("width",d).attr("height",i).attr("class","packetBlock"),c.append("text").attr("x",g+d/2).attr("y",u+i/2).attr("class","packetLabel").attr("dominant-baseline","middle").attr("text-anchor","middle").text(l.label),!b)continue;let k=l.end===l.start,f=u-2;c.append("text").attr("x",g+(k?d/2:0)).attr("y",f).attr("class","packetByte start").attr("dominant-baseline","auto").attr("text-anchor",k?"middle":"start").text(l.start),k||c.append("text").attr("x",g+d).attr("y",f).attr("class","packetByte end").attr("dominant-baseline","auto").attr("text-anchor","end").text(l.end)}},"drawWord"),q={draw:X},J={byteFontSize:"10px",startByteColor:"black",endByteColor:"black",labelColor:"black",labelFontSize:"12px",titleColor:"black",titleFontSize:"14px",blockStrokeColor:"black",blockStrokeWidth:"1",blockFillColor:"#efefef"},Q=n(({packet:t}={})=>{let e=x(J,t);return`
	.packetByte {
		font-size: ${e.byteFontSize};
	}
	.packetByte.start {
		fill: ${e.startByteColor};
	}
	.packetByte.end {
		fill: ${e.endByteColor};
	}
	.packetLabel {
		fill: ${e.labelColor};
		font-size: ${e.labelFontSize};
	}
	.packetTitle {
		fill: ${e.titleColor};
		font-size: ${e.titleFontSize};
	}
	.packetBlock {
		stroke: ${e.blockStrokeColor};
		stroke-width: ${e.blockStrokeWidth};
		fill: ${e.blockFillColor};
	}
	`},"styles"),rt={parser:U,db:h,renderer:q,styles:Q};export{rt as diagram};
