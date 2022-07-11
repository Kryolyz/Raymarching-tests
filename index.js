import * as t3 from "https://cdn.skypack.dev/three@0.132.2";
import { OrbitControls } from "https://cdn.skypack.dev/three@0.132.2/examples/jsm/controls/OrbitControls.js";

const scene = new t3.Scene();

const camera = new t3.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);

const renderer = new t3.WebGLRenderer( {
  canvas: document.querySelector('#bg'),
})

renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(window.innerWidth, window.innerHeight);

camera.position.setZ(30);

const geometry = new t3.TorusGeometry(10, 3, 16, 100)
const material = new t3.MeshStandardMaterial( {color: 0xE400FF} );
const torus = new t3.Mesh(geometry, material);

scene.add(torus);

const pointLight = new t3.PointLight(0xffffff);
pointLight.position.set(15,5,5);

const ambientLight = new t3.AmbientLight(0x323232);
scene.add(pointLight, ambientLight);

const lightHelper = new t3.PointLightHelper(pointLight);
scene.add(lightHelper);

const gridHelper = new t3.GridHelper(200, 50);
scene.add(gridHelper);

const controls = new OrbitControls(camera, renderer.domElement);

const backgroundTexture = new t3.TextureLoader().load('images/cg420_1.PNG');
scene.background = backgroundTexture;

function animate() {
  requestAnimationFrame(animate);

  torus.rotation.x += 0.01;
  torus.rotation.y += 0.005;
  torus.rotation.z += 0.005;

  controls.update();

  renderer.render(scene, camera);
}
animate();