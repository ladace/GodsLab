img = loadImage("assets/samples/test.png")

r = 0;

ox = getImageWidth(img) / 2;
oy = getImageHeight(img) / 2;

<<<<<<< HEAD
font = loadFont('Chinese.ttf', 20);

function draw()
    drawImageRgn(img, ox, oy, r, 0, 0, 300, 300, ox, oy, 640, 480)
    drawText(font, "哈哈OK", 0, 20);
    -- _drawText(100, 300, "ok", false)
=======
playMusic('Music.ogg')

function draw()
    -- drawImageRgn(img, ox, oy, r, 0, 0, 300, 300, ox, oy, 640, 480)
    _drawText(100, 300, "ok", false)
>>>>>>> parent of b3b23b5... Add better font supports now (via FTGL)
    r = r + 0.01
end

function onkey(code)
    print(code);
end