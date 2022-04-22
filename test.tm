fn double [x, _] {
    x -> _ >> or break;
    cycle {
        cycle { x>>; }
        _>>;
        cycle { x>>; }
        _ -> x <<;
        cycle { x<<; }
        _<<;
        cycle { x<<; }
        _ -> x >>;
        x -> _ >> or break;
    }
    _ -> x <<;
}