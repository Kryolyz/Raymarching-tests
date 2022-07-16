export default `
uniform vec2 resolution;
uniform float time;
uniform mat4 cameraTransform;
uniform vec3 lightPosition;

#define MIN_DIST 0.001
#define NUM_STEPS 64
#define SHADOW_STEPS 32
#define PI 3.14159

#define SHADOW_SOFTNESS 40.0

// Helper functions from https://github.com/nicoptere/raymarching-for-THREE/blob/master/glsl/fragment.glsl

vec2 unionAB(vec2 a, vec2 b){ if(a.x < b.x) return a; else return b;}
vec2 intersectionAB(vec2 a, vec2 b){return vec2(max(a.x, b.x),1.);}
vec2 blendAB( vec2 a, vec2 b, float t ){ return vec2(mix(a.x, b.x, t ),mix(a.y, b.y, t ));}
vec2 subtractAB(vec2 a, vec2 b){ return vec2(max(-a.x, b.x), a.y); }
//http://iquilezles.org/www/articles/smin/smin.htm
vec2 smin( vec2 a, vec2 b, float k ) { float h = clamp( 0.5+0.5*(b.x-a.x)/k, 0.0, 1.0 ); return vec2( mix( b.x, a.x, h ) - k*h*(1.0-h), mix( b.y, a.y, h ) ); }
 
mat3 rotationMatrix3(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat3(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c          );
}

vec2 sphere( vec3 p, float radius, vec3 pos, vec3 offset, vec4 quat)
{
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    float d = length( ( (   p-pos) * transform ) - offset) - radius;
    return vec2(d,1);
}

vec2 roundBox(vec3 p, vec3 size, float corner, vec3 pos, vec3 offset, vec4 quat )
{
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    return vec2( length( max( abs( (( p-pos ) * transform) - offset)-size, 0.0 ) )-corner,1.);
}

vec2 torus( vec3 p, vec2 radii, vec3 pos, vec3 offset, vec4 quat )
{
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    vec3 pp = ( p - pos ) * transform - offset;
    float d = length( vec2( length( pp.xz ) - radii.x, pp.y) ) - radii.y;
    return vec2(d,1.);
}

vec2 cone( vec3 p, vec2 c, vec3 pos, vec3 offset, vec4 quat  )
{
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    vec3 pp = ( p - pos ) * transform - offset;
    float q = length(pp.xy);
    return vec2( dot(c,vec2(q,pp.z)), 1. );
}

vec2 cylinder( vec3 p, float h, float r, vec3 pos, vec3 offset, vec4 quat ) {
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    vec3 pp = (p - pos ) * transform - offset;
    return vec2( max(length(pp.xz)-r, abs(pp.y)-h),1. );
}

float rand (vec2 st) {
    return fract(sin(dot(st.xy,
                         vec2(12.9898,78.233)))*
        43758.5453123);
}

vec2 orbitObject(vec3 p, vec3 pos) {
    // center object
    vec4 quat = vec4( 1., 0. , 0., time*0.02 );
    vec2 sp1 = sphere(p, 0.6, pos, vec3(0.), quat + vec4(0.));
    vec2 ring1 = torus( p, vec2( 2.5,.1), pos, vec3(0.), vec4( 1., 0. , 0., time*0.02 ));
    vec2 ring2 = torus( p, vec2( 3.0,.1), pos, vec3(0.), vec4( 0., 1. , 1., time*0.03 ));
    vec2 ring3 = torus( p, vec2( 4.0,.1), pos, vec3(0.), vec4( 1., 0. , 1., time*0.01 ));

    vec2 orbital = unionAB(sp1, ring1);
    orbital = unionAB(orbital, ring2);
    orbital = unionAB(orbital, ring3);

    for (int i = 1; i < 11; ++i) {
        vec3 offset = 3.0 * vec3(rand(vec2(float(i), float(i)*0.8)) - .5, rand(vec2(float(i)*3.2, float(i))) - .5, rand(vec2(float(i), float(i)*1.4)) - .5) * 5.;
        vec2 orbiter = sphere(p, 0.2, pos, offset, vec4(rand(vec2(float(i), float(i)*0.8)) - .5, rand(vec2(float(i), float(i)*1.4)) - .5, rand(vec2(float(i)*3.2, float(i))) - .5, time*0.01));
        orbital = unionAB(orbital, orbiter);
    }

    return orbital;
}

// Total signed distance field
vec2 sdf(vec3 p) {

    // sphere with orbits
    vec3 pos = vec3(0., 3., 0.0);
    vec2 orbital = orbitObject(p, pos);
    orbital.y = 0.1;

    // ground
    vec2 ground = vec2(p.y, 0.);
    // ground.y = 0.;

    vec3 boxPosition = vec3(0., 6. + 3. * cos(time * 0.01), 15.);
    float scale = mix(1., 2., smoothstep(2., 7., p.y));
    vec3 size = vec3(2., 3. , 2.);
    p.xz *= scale;
    boxPosition.xz *= scale;
    vec2 box = roundBox(p, size, .5, boxPosition, vec3(0.), vec4(1., 0., 0., 0.)) / scale;
    box.y = 0.0;

    // unionize everything to get one result
    // vec2 result = unionAB(orbital, ground);
    vec2 result = smin(orbital, ground, 1.);
    // result = unionAB(result, box);
    result = smin(result, box, 0.0);

    return result;
}
 
vec3 calcNormal(vec3 pos, float eps) {
    const vec3 v1 = vec3( 1.0,-1.0,-1.0);
    const vec3 v2 = vec3(-1.0,-1.0, 1.0);
    const vec3 v3 = vec3(-1.0, 1.0,-1.0);
    const vec3 v4 = vec3( 1.0, 1.0, 1.0);
  
    return normalize(
    v1 * sdf( pos + v1*eps ).x +
    v2 * sdf( pos + v2*eps ).x +
    v3 * sdf( pos + v3*eps ).x +
    v4 * sdf( pos + v4*eps ).x );
}  
vec3 calcNormal(vec3 pos) {
    return calcNormal(pos, 0.001);
}

vec3 getCamPosition() {
    return vec3(cameraTransform[3][0], cameraTransform[3][1], cameraTransform[3][2]);
}
mat3 getCamRotationMat() {
    mat3 rot = mat3(0.);
    for (int x = 0; x < 3; ++x) {
        for (int y = 0; y < 3; ++y) {
            rot[x][y] = cameraTransform[x][y];
        }
    }
    return rot;
}

vec3 rayMarch(vec3 pos, vec3 dir, bool light) {
	vec3 currentPosition;
    float dx = 0.;
    float closestPoint = 1.;
    vec2 temp = vec2(1000000.0,0.); // intentionally initialize to huge number
	for( int i = 0; i < NUM_STEPS; i++) {
 
        //update position along path
        currentPosition = pos + dir * dx;
 
        //gets the shortest distance to the scene
		temp = sdf( currentPosition );
 
        //break the loop if the distance was too small
        //this means that we are close enough to the surface
		if( abs(temp.x) < MIN_DIST || temp.x > 500.) break;

        // if(light && length(currentPosition - lightPosition) < 4.0) break;
 
		//increment the step along the ray path
		dx += temp.x;
	}
    return currentPosition;
}

float getLight(vec3 position) {
    vec3 normal = calcNormal(position);
    vec3 dir = normalize(lightPosition - position);

    return clamp(dot(normal, dir), 0., 1.);
}

// Sexy shadows by iq + improvement by sebastian aaltonen
float getShadows(vec3 origin) {
    vec2 dist;
    float result = 1.0;
    float ph = 1e20;
    float lightDist = length(lightPosition - origin);

    vec3 dir = normalize(lightPosition - origin);

    vec3 pos = origin + dir*MIN_DIST*15.;

    for (int i = 0; i < SHADOW_STEPS; ++i) {
        dist = sdf(pos);

        if (dist.x < MIN_DIST) 
            return 0.0;

        if (length(pos - origin) > lightDist) 
            return result;

        if (length(pos-origin) < lightDist) {
            float y = dist.x*dist.x / (2. * ph);
            float d = sqrt(dist.x*dist.x - y*y);
            // result = min(result, SHADOW_SOFTNESS * dist.x / length(pos-origin));
            result = min(result, SHADOW_SOFTNESS * d / max(0., length(pos-origin) - y));
            ph = dist.x;
        }
        pos+=dir*dist.x;
            
    }
    return result;
}

vec3 getMaterial(vec2 a, vec3 normal, vec3 position) {
    if (a.y < 0.1)
    //         return vec3((cos(position.x * PI / 2.) + cos(position.z * PI / 2.)) * (cos(position.x * PI / 2.) + cos(position.z * PI / 2.)) / 3.0);
        return mix(vec3(0.5), normal, 10.*a.y);
    if (a.y == 0.1) {
        return normal;
    }
    if (a.y == 0.2) {
        return normal;
    }
    return vec3(1.);
}

void main( void ) {
 
    // get screen uv's
	vec2 uv = ( gl_FragCoord.xy / resolution.xy ) * 2.0 - 1.0;
	uv.x *= resolution.x / resolution.y;
 
 
    // set camera position and direction

	vec3 pos = getCamPosition();
    mat3 cameraRot = getCamRotationMat();
	vec3 dir = cameraRot * normalize( vec3( uv, -1. ) );
 
    // raymarch
	vec3 final = rayMarch(pos, dir, false);
    vec2 val = sdf(final);
    
    // calc normals
    vec3 normal = calcNormal(final);

    // Assign material
    vec3 rgb = getMaterial(val, normal, final);

    rgb *= getLight(final);
    rgb *= getShadows(final);

    // Fix bugs and do background
    if (val.x > 10.)
    {
        vec3 col = vec3(dir.y*0.8+0.5, dir.y*0.8+0.5, 1.);
        rgb = col;
    }



	gl_FragColor = vec4(rgb, 1.);
}
`;