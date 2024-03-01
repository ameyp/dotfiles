var grid = slate.operation("grid", {
    "grids": {
        "3440x1440": {
            "width": 3,
            "height": 1
        },
        "1280x800": {
            "width": 2,
            "height": 1
        }
    }
});
slate.bind("1:ctrl", grid);
