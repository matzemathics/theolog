fn double [x, _] {
    x -> _ >> or break;
    cycle {
        cycle { x>> or break; }
        _>>;
        cycle { x>> or break; }
        _ -> x <<;
        cycle { x<< or break; }
        _<<;
        cycle { x<< or break; }
        _ -> x >>;
        x -> _ >> or break;
    }
    _ -> x <<;
}

fn multiply [1, 1', 2, 3, 4, 5, _] {
    branch 2 -> _ >> {
        cycle { 2 >> or break; }
    }

    1 -> 1' >> or break;
    cycle { [1, 2] >> or break; }
    _ <<;

    branch 1 -> _ << {
        cycle { 1 -> _ << or break; }
    }

    2 -> _ <<;
    branch 1 { }
    
    cycle {
        branch 5 -> 1 << {
            cycle { 5 -> 1 << or halt; }
        }

        2 -> _ <<;
        cycle {
            cycle { [2, 3, 5] << or break; }

            branch 1' >> {
                cycle { 3 -> 1 >> or 5 >> or break; }
                
                branch _ -> 1 << {
                    cycle { 5 -> 1 << or 1 << or break; }
                    1' -> 1;
                    halt;
                }

                2 -> 5 <<;
            }

            1 -> 3 >>;
            cycle { [3, 5] >> or break; }

            branch _ -> 5 { }

            2 -> 5 >>;
            cycle { 2 >> or break; }
            _ -> 2 <<;
        }

    }
}