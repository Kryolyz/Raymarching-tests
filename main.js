// import * as t3 from "https://cdn.skypack.dev/three@0.132.2";
import * as t3 from 'three';
// import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';
import { OrbitControls } from "https://unpkg.com/three@0.142.0/examples/jsm/controls/OrbitControls.js";

import fragment from './shaders/fragment.js';
import vertex from './shaders/vertex.js';

const scene = new t3.Scene();

const camera = new t3.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);

const renderer = new t3.WebGLRenderer( {
  canvas: document.querySelector('#bg'),
})

renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(window.innerWidth, window.innerHeight);

camera.position.setZ(30);

function createMesh() {
  const geometry = new t3.PlaneGeometry(2, 2);
  const material = new t3.ShaderMaterial({
    fragmentShader: fragment,
    vertexShader: vertex,
    uniforms:{
      time:{type:"f", value:0},
      resolution:{ type:"v2", value:new t3.Vector2( window.innerWidth, window.innerHeight) }}
    , side: t3.DoubleSide});
  const mesh = new t3.Mesh(geometry, material);
  return mesh;
}

var time = 0;

var plane = createMesh();
scene.add(plane);

const controls = new OrbitControls(camera, renderer.domElement);

function animate() {
  requestAnimationFrame(animate);

  time++;
  plane.material.uniforms.time.value = time;

  controls.update();
  renderer.render(scene, camera);
}
animate();