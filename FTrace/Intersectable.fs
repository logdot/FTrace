namespace FTrace.Types

type IIntersectable =
    abstract member intersect: Ray -> list<Intersection<IIntersectable>>
