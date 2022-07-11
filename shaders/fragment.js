export default `
uniform vec2 resolution;
uniform float time;

#define MIN_DIST 0.01
#define NUM_STEPS 32
#define PI 3.14159

// Helper functions from https://github.com/nicoptere/raymarching-for-THREE/blob/master/glsl/fragment.glsl
float unionAB(float a, float b){return min(a, b);}
float intersectionAB(float a, float b){return max(a, b);}
float blendAB( float a, float b, float t ){ return mix(a, b, t );}
float subtract(float a, float b){ return max(-a, b);}
//http://iquilezles.org/www/articles/smin/smin.htm
float smin( float a, float b, float k ) { float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 ); return mix( b, a, h ) - k*h*(1.0-h); }

mat3 calcLookAtMatrix(vec3 origin, vec3 target, float roll) {
    vec3 rr = vec3(sin(roll), cos(roll), 0.0);
    vec3 ww = normalize(target - origin);
    vec3 uu = normalize(cross(ww, rr));
    vec3 vv = normalize(cross(uu, ww));
    return mat3(uu, vv, ww);
}

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
 
float sphere(vec3 position, float radius, vec3 evalPosition) {
    return length(evalPosition - position) - radius;
}

float roundBox(vec3 p, vec3 size, float corner, vec3 pos, vec4 quat )
{
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    return length( max( abs( ( p-pos ) * transform )-size, 0.0 ) )-corner;
}

float torus( vec3 p, vec2 radii, vec3 pos, vec4 quat )
{
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    vec3 pp = ( p - pos ) * transform;
    float d = length( vec2( length( pp.xz ) - radii.x, pp.y ) ) - radii.y;
    return d;
}

float cone( vec3 p, vec2 c, vec3 pos, vec4 quat)
{
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    vec3 pp = ( p - pos ) * transform;
    float q = length(pp.xy);
    return dot(c,vec2(q,pp.z));
}

float cylinder( vec3 p, float h, float r, vec3 pos, vec4 quat ) {
    mat3 transform = rotationMatrix3( quat.xyz, quat.w );
    vec3 pp = (p - pos ) * transform;
    return max(length(pp.xz)-r, abs(pp.y)-h);
}

// Total signed distance field
float sdf(vec3 p) {
    float sph = sphere(vec3(cos(0.01*time), sin(0.01*time), .5), 2.0f, p);
    vec4 quat = vec4( 1., sin( time ) *.01 , 0., time * .02 );
    float rb = roundBox(p, vec3(1.0,1.0,2.0), 0.1, vec3(0.), quat + vec4( 1., 1., 1., PI / 4. ));

    float to0 = torus( p, vec2( 1.5,.9), vec3(0.), vec4( 1. + time * .01, 0. + time * .01, 0., 0. + time * .005 ) );

    float val = unionAB(sph, to0);
    val = intersectionAB(val, rb);
    return val;
}
 
vec3 calcNormal(vec3 pos, float eps) {
    const vec3 v1 = vec3( 1.0,-1.0,-1.0);
    const vec3 v2 = vec3(-1.0,-1.0, 1.0);
    const vec3 v3 = vec3(-1.0, 1.0,-1.0);
    const vec3 v4 = vec3( 1.0, 1.0, 1.0);
  
    return normalize(
    v1 * sdf( pos + v1*eps ) +
    v2 * sdf( pos + v2*eps ) +
    v3 * sdf( pos + v3*eps ) +
    v4 * sdf( pos + v4*eps ) );
}  
vec3 calcNormal(vec3 pos) {
    return calcNormal(pos, 0.002);
}

void main( void ) {
 
    // get screen uv's
	vec2 uv = ( gl_FragCoord.xy / resolution.xy ) * 2.0 - 1.0;
	uv.x *= resolution.x / resolution.y;
 
 
    // set camera position and direction
	vec3 pos = vec3( 0.,0.,-6.);
	vec3 dir = normalize( vec3( uv, 1. ) );
 
 
    // raymarching loop
    // Position of the ray during marching
	vec3 currentPosition;
 
	//variable step size
	float dx = 0.0;
    float temp = 1.0/0.0; // Yes, intentionally initialize to infinity
	for( int i = 0; i < NUM_STEPS; i++) {
 
        //update position along path
        currentPosition = pos + dir * dx;
 
        //gets the shortest distance to the scene
		temp = sdf( currentPosition );
 
        //break the loop if the distance was too small
        //this means that we are close enough to the surface
		if( temp < MIN_DIST ) break;
 
		//increment the step along the ray path
		dx += temp;
	}
    
    // calc normals
    vec3 normal = calcNormal(currentPosition.xyz);
    vec3 rgb = normal;

    // Fix bugs and do background
    if (temp > (MIN_DIST+0.001) || dot(dir, normal) > 0.0f )
    {
        vec3 col = vec3(dir.y*0.8+0.5, 0., 0.);
        rgb = col;
    }


	gl_FragColor = vec4(rgb, 1.0);
}
`;