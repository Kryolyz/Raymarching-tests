import * as t3 from 'three';
import { OrbitControls } from 'OrbitControls';

import fragment from './shaders/fragment.js';
import vertex from './shaders/vertex.js';
import { Vector3 } from 'three';

const scene = new t3.Scene();
const camera = new t3.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);

const renderer = new t3.WebGLRenderer( {
  minFilter: t3.LinearFilter,
  magFilter: t3.LinearFilter,
  canvas: document.querySelector('#bg'),
})

renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(window.innerWidth, window.innerHeight);

const controls = new OrbitControls(camera, renderer.domElement);

camera.position.setX(20);
camera.position.setY(10);

function createMesh() {
  const geometry = new t3.PlaneBufferGeometry(2, 2);

  var material = new t3.ShaderMaterial({
    fragmentShader: fragment,
    vertexShader: vertex,
    uniforms:{
      time:{type:"f", value:0},
      resolution:{ type:"v2", value:new t3.Vector2( window.innerWidth, window.innerHeight) },
      cameraTransform:{type:"mat4", value: camera.matrixWorld},
      lightPosition:{type:"vec3", value: new Vector3(5., 5., 5.)}
            }
    , side: t3.DoubleSide});
  material.dithering = true;
  const mesh = new t3.Mesh(geometry, material);
  return mesh;
}

var time = 0;
var plane = createMesh();
scene.add(plane);

camera.matrixAutoUpdate = true;
const fps = 60;

const texture = new t3.TextureLoader().load('3dtexture.bin');
console.log(texture.internalFormat);

function animate() {
  controls.update();

  time++;
  plane.material.uniforms.time.value = time;
  plane.material.uniforms.cameraTransform.value = camera.matrixWorld;
  plane.material.uniforms.lightPosition.value = new Vector3(35. * Math.cos(time*0.005), 35., 35. * Math.sin(time*0.005))

  if (Math.cos(time * 0.005) == 1) {
    time = 0;
  }
  renderer.render(scene, camera);
  setTimeout(() => {
    requestAnimationFrame(animate);
  }, 1000 / fps);
}
  
animate();