import * as t3 from 'three';
import { OrbitControls, MapControls } from 'OrbitControls';

import fragment from './shaders/fragment.js';
import vertex from './shaders/vertex.js';
import { Vector3 } from 'three';

const scene = new t3.Scene();
const camera = new t3.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);

const renderer = new t3.WebGLRenderer( {
  canvas: document.querySelector('#bg'),
})

renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(window.innerWidth, window.innerHeight);

const controls = new OrbitControls(camera, renderer.domElement);

camera.position.setX(20);
camera.position.setY(10);

function createMesh() {
  const geometry = new t3.PlaneGeometry(2, 2);
  const material = new t3.ShaderMaterial({
    fragmentShader: fragment,
    vertexShader: vertex,
    uniforms:{
      time:{type:"f", value:0},
      resolution:{ type:"v2", value:new t3.Vector2( window.innerWidth, window.innerHeight) },
      cameraTransform:{type:"mat4", value: camera.matrixWorld},
      lightPosition:{type:"vec3", value: new Vector3(5., 5., 5.)}
            }
    , side: t3.DoubleSide});
  const mesh = new t3.Mesh(geometry, material);
  return mesh;
}

var time = 0;
var plane = createMesh();
scene.add(plane);

camera.matrixAutoUpdate = true;

function animate() {
  requestAnimationFrame(animate);
  controls.update();

  time++;
  plane.material.uniforms.time.value = time;
  plane.material.uniforms.cameraTransform.value = camera.matrixWorld;
  plane.material.uniforms.lightPosition.value = new Vector3(18. * Math.cos(time*0.002), 20., 18. * Math.sin(time*0.002))

  renderer.render(scene, camera);
}
animate();