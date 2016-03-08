var svg = document.querySelector('svg');
var root = {
  dom: svg,
  x: 0,
  y: 0
};

var newNode = function(tagName){
  return document.createElementNS("http://www.w3.org/2000/svg", tagName);
};

var newTextNode = function(str){
  return document.createTextNode(str);
};

var newText = function(x, y, str, p){
  var text = {
    x: x + p.x,
    y: y + p.y
  };
  var dom = newNode('text');
  dom.setAttribute('x', text.x);
  dom.setAttribute('y', text.y);
  dom.setAttribute('text-anchor', 'middle');
  dom.setAttribute('alignment-baseline', 'middle');
  dom.appendChild(newTextNode(str));
  text.dom = dom;
  svg.appendChild(dom);
  return text;
};

var newBox = function(x, y, w, h, level, p, opts){
  if( !opts )
    opts = {};
  var box = {
    x: x + p.x,
    y: y + p.y,
    w: w,
    h: h
  };
  var i;
  var dom = [];
  for(i=0; i<level; ++i){
    dom.push(newNode('rect'));
    dom[i].setAttribute('x', box.x);
    dom[i].setAttribute('y', box.y + h/level*i);
    dom[i].setAttribute('width', w);
    dom[i].setAttribute('height', h/level);
    dom[i].setAttribute('stroke', opts.stroke || '#000');
    dom[i].setAttribute('fill', opts.fill || 'transparent');
    dom[i].setAttribute('stroke-width', opts.strokeWidth || 2);
    if( opts.text && opts.text[i] )
      newText(box.w/2, (h/level>=40 ? h/level*i+20 : h/level*(i+0.5)), opts.text[i], box);
    svg.appendChild(dom[i]);
  }
  box.dom = dom;

  box.link = [
    {x: box.x, y: box.y, d: Math.sqrt(box.w*box.w+box.h*box.h)/2},
    {x: box.x+box.w/2, y: box.y, d: box.h/2},
    {x: box.x+box.w, y: box.y, d: Math.sqrt(box.w*box.w+box.h*box.h)/2},
    {x: box.x+box.w, y: box.y+box.h/2, d: box.w/2},
    {x: box.x+box.w, y: box.y+box.h, d: Math.sqrt(box.w*box.w+box.h*box.h)/2},
    {x: box.x+box.w/2, y: box.y+box.h, d: box.h/2},
    {x: box.x, y: box.y+box.h, d: Math.sqrt(box.w*box.w+box.h*box.h)/2},
    {x: box.x, y: box.y+box.h/2, d: box.w/2}
  ];

  return box;
};

var newEllipse = function(x, y, w, h, p, opts){
  var box = {
    x: x + p.x,
    y: y + p.y,
    w: w,
    h: h
  };
  var dom = newNode('ellipse');
  dom.setAttribute('cx', box.x + box.w/2);
  dom.setAttribute('cy', box.y + box.h/2);
  dom.setAttribute('rx', box.w/2);
  dom.setAttribute('ry', box.h/2);
  box.dom = dom;

  if( opts.text )
    newText(w/2, h/2, opts.text, box);

  if( !opts )
    opts = {};
  dom.setAttribute('stroke', opts.stroke || '#000');
  dom.setAttribute('fill', opts.fill || 'transparent');
  dom.setAttribute('stroke-width', opts.strokeWidth || 2);

  svg.appendChild(dom);

  box.link = [
    {x: box.x+box.w/2, y: box.y, d: box.h/2},
    {x: box.x+box.w/2, y: box.y+box.h, d: box.h/2},
    {x: box.x, y: box.y+box.h/2, d: box.w/2},
    {x: box.x+box.w, y: box.y+box.h/2, d: box.w/2}
  ];
  return box;
};

var sqr = function(x){ return x*x };

var newLink = function(a, b, opts){
  var dis = 1e9, dis2;
  var i, j;
  var i0, j0;
  for(i=0; i<a.link.length; ++i)
    for(j=0; j<b.link.length; ++j){
      dis2 = Math.sqrt(sqr(a.link[i].x-b.link[j].x) + sqr(a.link[i].y-b.link[j].y)) + a.link[i].d + b.link[j].d;
      if( dis2 < dis ){
        i0 = i;
        j0 = j;
        dis = dis2;
      }
    }
  var dom = newNode('line');
  dom.setAttribute('x1', a.link[i0].x);
  dom.setAttribute('y1', a.link[i0].y);
  dom.setAttribute('x2', b.link[j0].x);
  dom.setAttribute('y2', b.link[j0].y);
  if( !opts )
    opts = {};
  dom.setAttribute('stroke', opts.stroke || '#000');
  dom.setAttribute('stroke-width', opts.strokeWidth || 2);

  var box = {};
  box.dom = dom;

  svg.appendChild(dom);

  return box;
}

//newBox(10, 10, 100, 100, root, {text: 'Hi'});
//newBox2(10, 130, 100, 100, root, {text1: 'A', text2: 'B'});
//newEllipse(250, 10, 200, 100, root, {text: 'ell'});
var world = newBox(450, 10, 100, 590, 1, root, {text: ['real world']});
var i;
var tvarValues = [1,3,'a',-4,2,'xx',3.5];
var tvars = [];
for(i=0; i<7; ++i)
  tvars.push(newBox(10, 40+i*70, 80, 60, 2, world, {text: ['TVar ' + (i+1), tvarValues[i]]}));

var trec1 = newBox(30, 15, 100, 150, 1, root, {text: ['TRec 1']});
  var entries1 = [];
  for(i=0; i<1; ++i)
    entries1.push(newBox(10, 40+i*100, 80, 90, 3, trec1, {text: ['TRecEntry', 1, 2]}));
var trec2_1 = newBox(200, 150, 100, 240, 1, root, {text: ['TRec 2-1']});
  var entries2_1_values = [[2,3], ['a','a']];
  var entries2_1 = [];
  for(i=0; i<2; ++i)
    entries2_1.push(newBox(10, 40+i*100, 80, 90, 3, trec2_1, {text: ['TRecEntry', entries2_1_values[i][0], entries2_1_values[i][1]]}));
var trec2_2 = newBox(200, 400, 100, 150, 1, root, {text: ['TRec 2-2']});
  var entries2_2 = [
    newBox(10, 40, 80, 90, 3, trec2_2, {text: ['TRecEntry', 'a', 'b']})
  ];
var trec3 = newBox(850, 250, 100, 300, 1, root, {text: ['TRec 3']});

var inv1 = newEllipse(750, 100, 80, 40, root, {text: 'inv 1'});
var inv2 = newEllipse(10, 50, 80, 40, trec3, {text: 'inv 2'});

var inv1trec = newBox(600, 120, 100, 40, 1, root, {text: ['TRec inv 1']});
newLink(inv1, inv1trec);

var inv2trec = newBox(700, 320, 100, 40, 1, root, {text: ['TRec inv 2']});
newLink(inv2, inv2trec);

newLink(trec2_1, trec2_2);
newLink(tvars[0], entries1[0]);
newLink(tvars[1], entries2_1[0]);
newLink(tvars[2], entries2_1[1]);
newLink(tvars[2], entries2_2[0]);

newLink(inv1trec, tvars[1]);
newLink(inv1trec, tvars[2]);

newLink(inv2trec, tvars[2]);
newLink(inv2trec, tvars[4]);
newLink(inv2trec, tvars[5]);
