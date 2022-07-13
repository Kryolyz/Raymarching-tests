import { MOUSE, Vector3 } from "three";

class Controller {

    constructor (object, domElement) {
        if ( domElement === undefined ) console.warn( 'THREE.OrbitControls: The second parameter "domElement" is now mandatory.' );
		if ( domElement === document ) console.error( 'THREE.OrbitControls: "document" should not be used as the target "domElement". Please use "renderer.domElement" instead.' );
        
        this.object = object;
		this.domElement = domElement;

        // Forward vector of camera
        this.forward = new Vector3();
        this.target = new Vector3();
        
		this.Arrowkeys = { LEFT: 'ArrowLeft', UP: 'ArrowUp', RIGHT: 'ArrowRight', BOTTOM: 'ArrowDown' };
        this.WASD = { LEFT: 'a', UP: 'w', RIGHT: 'd', BOTTOM: 's' };
        MOUSE.
		this.mouseButtons = { LEFT: MOUSE.LEFT, MIDDLE: MOUSE.MIDDLE, RIGHT: MOUSE.RIGHT };

    }

    function onMouseDown( event ) {

        let mouseAction;

        switch ( event.button ) {

            case 0:

                mouseAction = scope.mouseButtons.LEFT;
                break;

            case 1:

                mouseAction = scope.mouseButtons.MIDDLE;
                break;

            case 2:

                mouseAction = scope.mouseButtons.RIGHT;
                break;

            default:

                mouseAction = - 1;

        }

    function handleKeyDown( event ) {

        let needsUpdate = false;

        switch ( event.code ) {

            case scope.keys.UP:
                pan( 0, scope.keyPanSpeed );
                needsUpdate = true;
                break;

            case scope.keys.BOTTOM:
                pan( 0, - scope.keyPanSpeed );
                needsUpdate = true;
                break;

            case scope.keys.LEFT:
                pan( scope.keyPanSpeed, 0 );
                needsUpdate = true;
                break;

            case scope.keys.RIGHT:
                pan( - scope.keyPanSpeed, 0 );
                needsUpdate = true;
                break;

        }

        if ( needsUpdate ) {

            // prevent the browser from scrolling on cursor keys
            event.preventDefault();

            scope.update();

        }


    }


}