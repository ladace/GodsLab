img = loadImage("assets/samples/test.png")

r = 0;

ox = getImageWidth(img) / 2;
oy = getImageHeight(img) / 2;

playMusic('Music.ogg')

function draw()
    drawImageRgn(img, ox, oy, r, 0, 0, 300, 300, ox, oy, 640, 480)
    r = r + 0.01
end

function onkey(code)
    print(code);
end