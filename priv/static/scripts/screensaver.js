// !+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!
//src - https://codepen.io/yashbhardwaj/pen/hHgtE
import $ from 'jquery';
var timer;
var screan;
export function startScreensaver(){
    var canvas = document.createElement('canvas');
    screan = $('<div></div>', 
                    {css : {"z-index" : "99997",
                            "position" : "relative",
                            "height" : "100%",
                            "background" : "#000000"
                           }});
    $('#main-body').prepend(screan);
    screan.append(canvas);
    var FPS = 20;
    var F = 100;
    var N = 3;
    var VERTEX_MAX = 10;
    var TRAIL_QUALITY = 2000;
    var mu = 0.5;
    var bmRandom = function(mu, sigma){
        var x, y, r, tmp=null, tmp2;
        return function(){
            if(tmp !== null){
                tmp2 = tmp;
                tmp = null;
                return y*tmp2+mu;
            }
            do{
                x = Math.random()*2-1;
                y = Math.random()*2-1;
                r = x*x+y*y;
            }while(r>=1);
            tmp = sigma*Math.sqrt(-2*Math.log(r)/r);
            return x*tmp+mu;
        };
    };
    var pointCopy = function(src, dst){
        dst.x = src.x;
        dst.y = src.y;
        dst.z = src.z;
        return dst;
    };
    var Trail = function(pos, t, color_f){
        this.pos={x:0,y:0,z:0};
        this.start={x:0,y:0,z:0};
        this.goal={x:0,y:0,z:0};
        this.anchor_1={x:0,y:0,z:0};
        this.anchor_2={x:0,y:0,z:0};
        this.start_time = 0;
        this.take_time = 1;
        this.vertexes = [];
        this.anchors_1 = [];
        this.anchors_2 = [];
        this.color_f = color_f;
        pointCopy(pos, this.pos);
        pointCopy(pos, this.start);
        pointCopy(pos, this.goal);
        this.setNextGoal(t);
    };
    Trail.prototype.setNextGoal = function(t, target){
        pointCopy(this.goal, this.start);
        this.anchor_1.x = this.start.x+(this.start.x-this.anchor_2.x)*mu;
        this.anchor_1.y = this.start.y+(this.start.y-this.anchor_2.y)*mu;
        this.anchor_1.z = this.start.z+(this.start.z-this.anchor_2.z)*mu;
        if(target){
            this.anchor_2.x = (this.anchor_1.x+target.x)/2+myrand();
            this.anchor_2.y = (this.anchor_1.y+target.y)/2+myrand();
            this.anchor_2.z = (this.anchor_1.z+target.z)/2+myrand();
            this.goal.x = target.x;
            this.goal.y = target.y;
            this.goal.z = target.z;
        }else{
            this.anchor_2.x = this.anchor_1.x+myrand();
            this.anchor_2.y = this.anchor_1.y+myrand();
            this.anchor_2.z = this.anchor_1.z+myrand();
            this.goal.x = this.anchor_2.x+myrand();
            this.goal.y = this.anchor_2.y+myrand();
            this.goal.z = this.anchor_2.z+myrand();
        }
        this.start_time = t;
        this.take_time = 200+Math.random()*200;
        this.vertexes.push(pointCopy(this.start, {x:0,y:0,z:0}));
        this.anchors_1.push(pointCopy(this.anchor_1, {x:0,y:0,z:0}));
        this.anchors_2.push(pointCopy(this.anchor_2, {x:0,y:0,z:0}));
        if(this.vertexes.length > VERTEX_MAX){
            this.vertexes.splice(0,this.vertexes.length-VERTEX_MAX);
            this.anchors_1.splice(0,this.anchors_1.length-VERTEX_MAX);
            this.anchors_2.splice(0,this.anchors_2.length-VERTEX_MAX);
        }
    };
    Trail.prototype.update = function(t, target){
        bezier3(
            t-this.start_time,
            this.start,
            this.anchor_1,
            this.anchor_2,
            this.goal,
            this.take_time,
            this.pos
            );
        if(t-this.start_time > this.take_time){
            this.setNextGoal(this.start_time+this.take_time, target);
            this.update(t, target);
        }
    };
    Trail.prototype.draw = function(ctx, camera, t){
        var i, dz, dt, ddt, rt, a, v={x:0, y:0, z:0};
        var ps = {x:0, y:0};
        ctx.beginPath();
        if(perspective(this.vertexes[0], camera, ps)){
            ctx.moveTo(ps.x, ps.y);
        }
        rt = (t-this.start_time)/this.take_time;
        for(i=1; i<this.vertexes.length; i++){
            ddt = 0.01;
            for(dt=0; dt<1; dt+=ddt){
                bezier3(dt,
                        this.vertexes[i-1],
                        this.anchors_1[i-1],
                        this.anchors_2[i-1],
                        this.vertexes[i],
                        1,
                        v);
                if(perspective(v, camera, ps)){
                    dz = v.z-camera.z;
                    a = 1-(this.vertexes.length-i+1-dt+rt)/VERTEX_MAX;
                    this.color_f(ctx, a, dz);
                    ctx.lineTo(ps.x, ps.y);
                    ctx.stroke();
                    ctx.beginPath();
                    ctx.moveTo(ps.x, ps.y);
                    ddt = dz/TRAIL_QUALITY+0.01;
                }
            }
        }
        ddt = 0.01;
        for(dt=0; dt<rt; dt+=ddt){
            bezier3(dt,
                    this.start,
                    this.anchor_1,
                    this.anchor_2,
                    this.goal,
                    1,
                    v);
            if(perspective(v, camera, ps)){
                dz = v.z-camera.z;
                a = 1-(1-dt+rt)/VERTEX_MAX;
                this.color_f(ctx, a, dz);
                ctx.lineTo(ps.x, ps.y);
                ctx.stroke();
                ctx.beginPath();
                ctx.moveTo(ps.x, ps.y);
                ddt = dz/TRAIL_QUALITY+0.01;
            }
        }
        if(perspective(this.pos, camera, ps)){
            dz = this.pos.z-camera.z;
            a = 1-1/VERTEX_MAX;
            this.color_f(ctx, a, dz);
            ctx.lineTo(ps.x, ps.y);
            ctx.stroke();
        }
    };
    var bezier3 = function(t, a, b, c, d, e, dst){
        t /= e;
        dst.x = 
            a.x*(1-t)*(1-t)*(1-t)+
            b.x*3*t*(1-t)*(1-t)+
            c.x*3*t*t*(1-t)+
            d.x*t*t*t;
        dst.y = 
            a.y*(1-t)*(1-t)*(1-t)+
            b.y*3*t*(1-t)*(1-t)+
            c.y*3*t*t*(1-t)+
            d.y*t*t*t;
        dst.z = 
            a.z*(1-t)*(1-t)*(1-t)+
            b.z*3*t*(1-t)*(1-t)+
            c.z*3*t*t*(1-t)+
            d.z*t*t*t;
    };
    var perspective = function(point, camera, dst){
        var dx = point.x-camera.x;
        var dy = point.y-camera.y;
        var dz = point.z-camera.z;
        if(dz > 0){
            dst.x = F*dx/dz;
            dst.y = F*dy/dz;
            return true;
        }
        return false;
    };
    var updateScene = function(){
        var i;
        time_now = new Date().getTime();
        var time_d = time_now-time_pre;
        trails[0].update(time_now);
        for(i=1; i<trails.length; i++){
            trails[i].update(time_now, trails[i-1].pos);
        }
        camera.x += (trails[0].pos.x-camera.x)*0.0005*time_d;
        camera.y += (trails[0].pos.y-camera.y)*0.0005*time_d;
        camera.z += (trails[0].pos.z-camera.z-100)*0.0005*time_d;
        time_pre = time_now;
    };
    var drawScene = function(ctx){
        var i;
        ctx.clearRect(-canvas.width/2, -canvas.height/2, canvas.width, canvas.height);
        for(i=0; i<trails.length; i++){
            trails[i].draw(ctx, camera, time_now);
        }
    };
    var myrand = bmRandom(0,20);
    // var canvas = document.getElementsByTagName("canvas")[0];
    var ctx = canvas.getContext("2d");
    var trails = [];
    var i;
    var time_now = new Date().getTime();
    var time_pre = time_now;
    var camera = {x:0, y:0, z:-200};
    var whiteFun = function(){return "#FFFFFF";};
    for(i=0; i<N; i++){
        trails.push(new Trail({x:myrand(), y:myrand(), z:myrand()},
                              time_now, whiteFun));
    }

    var trails1 = function(ctx, a, dz){
                    var b = dz<10?0:a*F/dz;
                    b = (b>1?1:b)*(dz<30?(dz-10)/20:1);
                    ctx.strokeStyle = "rgba(255,"+Math.floor(255*a)+",0,"+b+")";
                    ctx.lineWidth = F/dz;
                    ctx.lineCap = b>0.8?"round":"butt";
                };
    var trails2 = function(ctx, a, dz){
                    var b = dz<10?0:a*F/dz;
                    b = (b>1?1:b)*(dz<30?(dz-10)/20:1);
                    ctx.strokeStyle = "rgba(0, 255,"+Math.floor(255*a)+","+b+")";
                    ctx.lineWidth = F/dz;
                    ctx.lineCap = b>0.8?"round":"butt";
                };
    var trails3 = function(ctx, a, dz){
                    var b = dz<10?0:a*F/dz;
                    b = (b>1?1:b)*(dz<30?(dz-10)/20:1);
                    ctx.strokeStyle = "rgba("+Math.floor(255*a)+",0,255,"+b+")";
                    ctx.lineWidth = F/dz;
                    ctx.lineCap = b>0.8?"round":"butt";
                };

    for(i=0; i<N; i++){
        switch(i%3){
            case 0:
                trails[i].color_f= trails1;
                break;
            case 1:
                trails[i].color_f= trails2;
                break;
            default:
                trails[i].color_f= trails3;
                break;
        }
    }
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    ctx.translate(canvas.width/2, canvas.height/2);
    timer = setInterval(function(){
        updateScene();
        drawScene(ctx);
    }, 1000/FPS);
}

export function stopScreensaver() {
    screan.remove();
    clearInterval(timer);
}
