function _iterableToArrayLimit(r, l) {
  var t = null == r ? null : "undefined" != typeof Symbol && r[Symbol.iterator] || r["@@iterator"];
  if (null != t) {
    var e,
      n,
      i,
      u,
      a = [],
      f = !0,
      o = !1;
    try {
      if (i = (t = t.call(r)).next, 0 === l) {
        if (Object(t) !== t) return;
        f = !1;
      } else for (; !(f = (e = i.call(t)).done) && (a.push(e.value), a.length !== l); f = !0);
    } catch (r) {
      o = !0, n = r;
    } finally {
      try {
        if (!f && null != t.return && (u = t.return(), Object(u) !== u)) return;
      } finally {
        if (o) throw n;
      }
    }
    return a;
  }
}
function ownKeys(e, r) {
  var t = Object.keys(e);
  if (Object.getOwnPropertySymbols) {
    var o = Object.getOwnPropertySymbols(e);
    r && (o = o.filter(function (r) {
      return Object.getOwnPropertyDescriptor(e, r).enumerable;
    })), t.push.apply(t, o);
  }
  return t;
}
function _objectSpread2(e) {
  for (var r = 1; r < arguments.length; r++) {
    var t = null != arguments[r] ? arguments[r] : {};
    r % 2 ? ownKeys(Object(t), !0).forEach(function (r) {
      _defineProperty(e, r, t[r]);
    }) : Object.getOwnPropertyDescriptors ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t)) : ownKeys(Object(t)).forEach(function (r) {
      Object.defineProperty(e, r, Object.getOwnPropertyDescriptor(t, r));
    });
  }
  return e;
}
function _toPrimitive(t, r) {
  if ("object" != typeof t || !t) return t;
  var e = t[Symbol.toPrimitive];
  if (void 0 !== e) {
    var i = e.call(t, r || "default");
    if ("object" != typeof i) return i;
    throw new TypeError("@@toPrimitive must return a primitive value.");
  }
  return ("string" === r ? String : Number)(t);
}
function _toPropertyKey(t) {
  var i = _toPrimitive(t, "string");
  return "symbol" == typeof i ? i : String(i);
}
function _typeof(o) {
  "@babel/helpers - typeof";

  return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (o) {
    return typeof o;
  } : function (o) {
    return o && "function" == typeof Symbol && o.constructor === Symbol && o !== Symbol.prototype ? "symbol" : typeof o;
  }, _typeof(o);
}
function _classCallCheck(instance, Constructor) {
  if (!(instance instanceof Constructor)) {
    throw new TypeError("Cannot call a class as a function");
  }
}
function _defineProperties(target, props) {
  for (var i = 0; i < props.length; i++) {
    var descriptor = props[i];
    descriptor.enumerable = descriptor.enumerable || false;
    descriptor.configurable = true;
    if ("value" in descriptor) descriptor.writable = true;
    Object.defineProperty(target, _toPropertyKey(descriptor.key), descriptor);
  }
}
function _createClass(Constructor, protoProps, staticProps) {
  if (protoProps) _defineProperties(Constructor.prototype, protoProps);
  if (staticProps) _defineProperties(Constructor, staticProps);
  Object.defineProperty(Constructor, "prototype", {
    writable: false
  });
  return Constructor;
}
function _defineProperty(obj, key, value) {
  key = _toPropertyKey(key);
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }
  return obj;
}
function _inherits(subClass, superClass) {
  if (typeof superClass !== "function" && superClass !== null) {
    throw new TypeError("Super expression must either be null or a function");
  }
  subClass.prototype = Object.create(superClass && superClass.prototype, {
    constructor: {
      value: subClass,
      writable: true,
      configurable: true
    }
  });
  Object.defineProperty(subClass, "prototype", {
    writable: false
  });
  if (superClass) _setPrototypeOf(subClass, superClass);
}
function _getPrototypeOf(o) {
  _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf(o) {
    return o.__proto__ || Object.getPrototypeOf(o);
  };
  return _getPrototypeOf(o);
}
function _setPrototypeOf(o, p) {
  _setPrototypeOf = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf(o, p) {
    o.__proto__ = p;
    return o;
  };
  return _setPrototypeOf(o, p);
}
function _isNativeReflectConstruct() {
  if (typeof Reflect === "undefined" || !Reflect.construct) return false;
  if (Reflect.construct.sham) return false;
  if (typeof Proxy === "function") return true;
  try {
    Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {}));
    return true;
  } catch (e) {
    return false;
  }
}
function _assertThisInitialized(self) {
  if (self === void 0) {
    throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
  }
  return self;
}
function _possibleConstructorReturn(self, call) {
  if (call && (typeof call === "object" || typeof call === "function")) {
    return call;
  } else if (call !== void 0) {
    throw new TypeError("Derived constructors may only return object or undefined");
  }
  return _assertThisInitialized(self);
}
function _createSuper(Derived) {
  var hasNativeReflectConstruct = _isNativeReflectConstruct();
  return function _createSuperInternal() {
    var Super = _getPrototypeOf(Derived),
      result;
    if (hasNativeReflectConstruct) {
      var NewTarget = _getPrototypeOf(this).constructor;
      result = Reflect.construct(Super, arguments, NewTarget);
    } else {
      result = Super.apply(this, arguments);
    }
    return _possibleConstructorReturn(this, result);
  };
}
function _superPropBase(object, property) {
  while (!Object.prototype.hasOwnProperty.call(object, property)) {
    object = _getPrototypeOf(object);
    if (object === null) break;
  }
  return object;
}
function _get() {
  if (typeof Reflect !== "undefined" && Reflect.get) {
    _get = Reflect.get.bind();
  } else {
    _get = function _get(target, property, receiver) {
      var base = _superPropBase(target, property);
      if (!base) return;
      var desc = Object.getOwnPropertyDescriptor(base, property);
      if (desc.get) {
        return desc.get.call(arguments.length < 3 ? target : receiver);
      }
      return desc.value;
    };
  }
  return _get.apply(this, arguments);
}
function _slicedToArray(arr, i) {
  return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest();
}
function _toConsumableArray(arr) {
  return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray(arr) || _nonIterableSpread();
}
function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}
function _arrayWithHoles(arr) {
  if (Array.isArray(arr)) return arr;
}
function _iterableToArray(iter) {
  if (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null || iter["@@iterator"] != null) return Array.from(iter);
}
function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen);
}
function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;
  for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i];
  return arr2;
}
function _nonIterableSpread() {
  throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}
function _nonIterableRest() {
  throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}
function _createForOfIteratorHelper(o, allowArrayLike) {
  var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"];
  if (!it) {
    if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") {
      if (it) o = it;
      var i = 0;
      var F = function () {};
      return {
        s: F,
        n: function () {
          if (i >= o.length) return {
            done: true
          };
          return {
            done: false,
            value: o[i++]
          };
        },
        e: function (e) {
          throw e;
        },
        f: F
      };
    }
    throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
  }
  var normalCompletion = true,
    didErr = false,
    err;
  return {
    s: function () {
      it = it.call(o);
    },
    n: function () {
      var step = it.next();
      normalCompletion = step.done;
      return step;
    },
    e: function (e) {
      didErr = true;
      err = e;
    },
    f: function () {
      try {
        if (!normalCompletion && it.return != null) it.return();
      } finally {
        if (didErr) throw err;
      }
    }
  };
}

var t = {};
function MakeIntrinsicClass(e, t) {
  Object.defineProperty(e.prototype, Symbol.toStringTag, {
    value: t,
    writable: !1,
    enumerable: !1,
    configurable: !0
  });
  var _iterator = _createForOfIteratorHelper(Object.getOwnPropertyNames(e)),
    _step;
  try {
    for (_iterator.s(); !(_step = _iterator.n()).done;) {
      var _t = _step.value;
      var _r = Object.getOwnPropertyDescriptor(e, _t);
      _r.configurable && _r.enumerable && (_r.enumerable = !1, Object.defineProperty(e, _t, _r));
    }
  } catch (err) {
    _iterator.e(err);
  } finally {
    _iterator.f();
  }
  var _iterator2 = _createForOfIteratorHelper(Object.getOwnPropertyNames(e.prototype)),
    _step2;
  try {
    for (_iterator2.s(); !(_step2 = _iterator2.n()).done;) {
      var _t2 = _step2.value;
      var _r2 = Object.getOwnPropertyDescriptor(e.prototype, _t2);
      _r2.configurable && _r2.enumerable && (_r2.enumerable = !1, Object.defineProperty(e.prototype, _t2, _r2));
    }
  } catch (err) {
    _iterator2.e(err);
  } finally {
    _iterator2.f();
  }
  DefineIntrinsic(t, e), DefineIntrinsic("".concat(t, ".prototype"), e.prototype);
}
function DefineIntrinsic(e, r) {
  var o = "%".concat(e, "%");
  if (void 0 !== t[o]) throw new Error("intrinsic ".concat(e, " already exists"));
  t[o] = r;
}
function GetIntrinsic(e) {
  return t[e];
}
var r, o;
var n = "slot-epochNanoSeconds",
  a = "slot-timezone-identifier",
  i = "slot-year",
  s = "slot-month",
  l = "slot-day",
  d = "slot-hour",
  m = "slot-minute",
  c = "slot-second",
  h = "slot-millisecond",
  u = "slot-microsecond",
  T = "slot-nanosecond",
  p = "slot-calendar",
  f = "slot-date-brand",
  y = "slot-year-month-brand",
  I = "slot-month-day-brand",
  S = "slot-cached-instant",
  g = "slot-time-zone",
  w = "slot-years",
  D = "slot-months",
  G = "slot-weeks",
  v = "slot-days",
  C = "slot-hours",
  O = "slot-minutes",
  b = "slot-seconds",
  E = "slot-milliseconds",
  M = "slot-microseconds",
  R = "slot-nanoseconds",
  F = "slot-calendar-identifier",
  Y = new WeakMap();
var P = Symbol["for"]("@@Temporal__GetSlots");
(r = globalThis)[P] || (r[P] = function _GetSlots(e) {
  return Y.get(e);
});
var Z = globalThis[P];
var B = Symbol["for"]("@@Temporal__CreateSlots");
(o = globalThis)[B] || (o[B] = function _CreateSlots(e) {
  Y.set(e, Object.create(null));
});
var N = globalThis[B];
function HasSlot(e) {
  if (!e || "object" != _typeof(e)) return !1;
  var r = Z(e);
  for (var _len = arguments.length, t = new Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
    t[_key - 1] = arguments[_key];
  }
  return !!r && t.every(function (e) {
    return e in r;
  });
}
function GetSlot(e, t) {
  var _Z;
  var r = (_Z = Z(e)) === null || _Z === void 0 ? void 0 : _Z[t];
  if (void 0 === r) throw new TypeError("Missing internal slot ".concat(t));
  return r;
}
function SetSlot(e, t, r) {
  var o = Z(e);
  if (void 0 === o) throw new TypeError("Missing slots for the given container");
  if (o[t]) throw new TypeError("".concat(t, " already has set"));
  o[t] = r;
}
var j = /\.[-A-Za-z_]|\.\.[-A-Za-z._]{1,12}|\.[-A-Za-z_][-A-Za-z._]{0,12}|[A-Za-z_][-A-Za-z._]{0,13}/,
  $ = new RegExp("(?:" + ["(?:".concat(j.source, ")(?:\\/(?:").concat(j.source, "))*"), "Etc/GMT(?:0|[-+]\\d{1,2})", "GMT[-+]?0", "EST5EDT", "CST6CDT", "MST7MDT", "PST8PDT", /(?:[+\u2212-][0-2][0-9](?::?[0-5][0-9](?::?[0-5][0-9](?:[.,]\d{1,9})?)?)?)/.source].join("|") + ")"),
  k = /(?:[+\u2212-]\d{6}|\d{4})/,
  U = /(?:0[1-9]|1[0-2])/,
  A = /(?:0[1-9]|[12]\d|3[01])/,
  L = new RegExp("(".concat(k.source, ")(?:-(").concat(U.source, ")-(").concat(A.source, ")|(").concat(U.source, ")(").concat(A.source, "))")),
  x = /(\d{2})(?::(\d{2})(?::(\d{2})(?:[.,](\d{1,9}))?)?|(\d{2})(?:(\d{2})(?:[.,](\d{1,9}))?)?)?/,
  W = /([+\u2212-])([01][0-9]|2[0-3])(?::?([0-5][0-9])(?::?([0-5][0-9])(?:[.,](\d{1,9}))?)?)?/,
  q = new RegExp("([zZ])|".concat(W.source, "?")),
  H = /\[(!)?([a-z_][a-z0-9_-]*)=([A-Za-z0-9]+(?:-[A-Za-z0-9]+)*)\]/g,
  V = new RegExp(["^".concat(L.source), "(?:(?:T|\\s+)".concat(x.source, "(?:").concat(q.source, ")?)?"), "(?:\\[!?(".concat($.source, ")\\])?"), "((?:".concat(H.source, ")*)$")].join(""), "i"),
  z = new RegExp(["^T?".concat(x.source), "(?:".concat(q.source, ")?"), "(?:\\[!?".concat($.source, "\\])?"), "((?:".concat(H.source, ")*)$")].join(""), "i"),
  _ = new RegExp("^(".concat(k.source, ")-?(").concat(U.source, ")(?:\\[!?").concat($.source, "\\])?((?:").concat(H.source, ")*)$")),
  J = new RegExp("^(?:--)?(".concat(U.source, ")-?(").concat(A.source, ")(?:\\[!?").concat($.source, "\\])?((?:").concat(H.source, ")*)$")),
  K = /(\d+)(?:[.,](\d{1,9}))?/,
  X = new RegExp("(?:".concat(K.source, "H)?(?:").concat(K.source, "M)?(?:").concat(K.source, "S)?")),
  Q = new RegExp("^([+\u2212-])?P".concat(/(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)W)?(?:(\d+)D)?/.source, "(?:T(?!$)").concat(X.source, ")?$"), "i"),
  ee = Array.prototype.includes,
  te = Array.prototype.push,
  re = globalThis.Intl.DateTimeFormat,
  oe = Math.min,
  ne = Math.max,
  ae = Math.abs,
  ie = Math.floor,
  se = Math.sign,
  le = Math.trunc,
  de = Number.isNaN,
  me = Number.isFinite,
  ce = Number,
  he = String,
  ue = Number.MAX_SAFE_INTEGER,
  Te = Object.create,
  pe = Object.getOwnPropertyDescriptor,
  fe = Reflect.apply,
  ye = Reflect.ownKeys,
  Ie = 0n,
  Se = 1n,
  ge = 60n,
  we = 24n,
  De = 1000n,
  Ge = 1000000n,
  ve = 1000000000n,
  Ce = BigInt(-1),
  Oe = 3600n * ve,
  be = ge * ve,
  Ee = Oe * we,
  Me = BigInt(-86400) * 100000000000000000n,
  Re = 86400n * 100000000000000000n,
  Fe = -271821,
  Ye = 275760,
  Pe = BigInt(-388152) * 10000000000000n,
  Ze = Ee * 3660n,
  Be = Ee * 366n,
  Ne = Ee * 14n,
  je = ["iso8601", "hebrew", "islamic", "islamic-umalqura", "islamic-tbla", "islamic-civil", "islamic-rgsa", "islamicc", "persian", "ethiopic", "ethioaa", "coptic", "chinese", "dangi", "roc", "indian", "buddhist", "japanese", "gregory"];
function isZero(t) {
  return t === Ie;
}
function GetMethod(e, t) {
  var r = e[t];
  if (void 0 !== r) return r;
}
function Call(e, t, r) {
  var o = arguments.length > 2 ? r : [];
  return fe(e, t, o);
}
function IsObject(e) {
  return "object" == _typeof(e) && null !== e || "function" == typeof e;
}
function ToNumber(e) {
  if ("bigint" == typeof e) throw new TypeError("Cannot convert BigInt to number");
  return ce(e);
}
function ToIntegerOrInfinity(e) {
  var t = ToNumber(e);
  if (de(t) || 0 === t) return 0;
  if (!me(t)) return t;
  var r = ie(ae(t));
  return 0 === r ? 0 : se(t) * r;
}
function IsIntegralNumber(e) {
  if ("number" != typeof e || de(e) || !me(e)) return !1;
  var t = ae(e);
  return ie(t) === t;
}
function ToString(e) {
  if ("symbol" == _typeof(e)) throw new TypeError("Cannot convert a Symbol value to a String");
  return he(e);
}
function ToIntegerWithTruncation(e) {
  var t = ToNumber(e);
  if (0 === t) return 0;
  if (de(t) || !me(t)) throw new RangeError("invalid number value");
  var r = le(t);
  return 0 === r ? 0 : r;
}
function ToPositiveIntegerWithTruncation(e, t) {
  var r = ToIntegerWithTruncation(e);
  if (r <= 0) {
    if (void 0 !== t) throw new RangeError("property '".concat(t, "' cannot be a a number less than one"));
    throw new RangeError("Cannot convert a number less than one to a positive integer");
  }
  return r;
}
function ToIntegerIfIntegral(e) {
  var t = ToNumber(e);
  if (!me(t)) throw new RangeError("infinity is out of range");
  if (!IsIntegralNumber(t)) throw new RangeError("unsupported fractional value ".concat(e));
  return 0 === t ? 0 : t;
}
function divmod(t, r) {
  return {
    quotient: t / r,
    remainder: t % r
  };
}
function isNegativeJSBI(t) {
  return t < Ie;
}
function signJSBI(e) {
  return isZero(e) ? 0 : isNegativeJSBI(e) ? -1 : 1;
}
function abs(t) {
  return t < Ie ? t * Ce : t;
}
var $e = new Map([["year", ToIntegerWithTruncation], ["month", ToPositiveIntegerWithTruncation], ["monthCode", ToString], ["day", ToPositiveIntegerWithTruncation], ["hour", ToIntegerWithTruncation], ["minute", ToIntegerWithTruncation], ["second", ToIntegerWithTruncation], ["millisecond", ToIntegerWithTruncation], ["microsecond", ToIntegerWithTruncation], ["nanosecond", ToIntegerWithTruncation], ["years", ToIntegerIfIntegral], ["months", ToIntegerIfIntegral], ["weeks", ToIntegerIfIntegral], ["days", ToIntegerIfIntegral], ["hours", ToIntegerIfIntegral], ["minutes", ToIntegerIfIntegral], ["seconds", ToIntegerIfIntegral], ["milliseconds", ToIntegerIfIntegral], ["microseconds", ToIntegerIfIntegral], ["nanoseconds", ToIntegerIfIntegral], ["era", ToString], ["eraYear", ToIntegerOrInfinity], ["offset", ToString]]),
  ke = new Map([["hour", 0], ["minute", 0], ["second", 0], ["millisecond", 0], ["microsecond", 0], ["nanosecond", 0]]),
  Ue = [["years", "year", "date"], ["months", "month", "date"], ["weeks", "week", "date"], ["days", "day", "date"], ["hours", "hour", "time"], ["minutes", "minute", "time"], ["seconds", "second", "time"], ["milliseconds", "millisecond", "time"], ["microseconds", "microsecond", "time"], ["nanoseconds", "nanosecond", "time"]],
  Ae = new Map(Ue.map(function (e) {
    return [e[0], e[1]];
  })),
  Le = new Map(Ue.map(function (_ref) {
    var _ref2 = _slicedToArray(_ref, 2),
      e = _ref2[0],
      t = _ref2[1];
    return [t, e];
  })),
  xe = Ue.map(function (_ref3) {
    var _ref4 = _slicedToArray(_ref3, 2),
      e = _ref4[1];
    return e;
  }),
  We = Array.from(Ae.keys()).sort(),
  qe = new Map();
function getIntlDateTimeFormatEnUsForTimeZone(e) {
  var t = qe.get(e);
  return void 0 === t && (t = new re("en-us", {
    timeZone: he(e),
    hour12: !1,
    era: "short",
    year: "numeric",
    month: "numeric",
    day: "numeric",
    hour: "numeric",
    minute: "numeric",
    second: "numeric"
  }), qe.set(e, t)), t;
}
function ToObject(e) {
  if (null == e) throw new TypeError("Expected object not ".concat(e));
  return Object(e);
}
function CopyDataProperties(e, t, r, o) {
  if (null == t) return;
  var n = ye(t);
  var _iterator3 = _createForOfIteratorHelper(n),
    _step3;
  try {
    var _loop = function _loop() {
      var a = _step3.value;
      if (!r.some(function (e) {
        return Object.is(e, a);
      }) && Object.prototype.propertyIsEnumerable.call(t, a)) {
        var _r3 = t[a];
        if (o && o.some(function (e) {
          return Object.is(e, _r3);
        })) return 1; // continue
        e[a] = _r3;
      }
    };
    for (_iterator3.s(); !(_step3 = _iterator3.n()).done;) {
      if (_loop()) continue;
    }
  } catch (err) {
    _iterator3.e(err);
  } finally {
    _iterator3.f();
  }
}
function IsTemporalInstant(e) {
  return HasSlot(e, n) && !HasSlot(e, g, p);
}
function IsTemporalTimeZone(e) {
  return HasSlot(e, a);
}
function IsTemporalCalendar(e) {
  return HasSlot(e, F);
}
function IsTemporalDuration(e) {
  return HasSlot(e, w, D, v, C, O, b, E, M, R);
}
function IsTemporalDate(e) {
  return HasSlot(e, f);
}
function IsTemporalTime(e) {
  return HasSlot(e, d, m, c, h, u, T) && !HasSlot(e, i, s, l);
}
function IsTemporalDateTime(e) {
  return HasSlot(e, i, s, l, d, m, c, h, u, T);
}
function IsTemporalYearMonth(e) {
  return HasSlot(e, y);
}
function IsTemporalMonthDay(e) {
  return HasSlot(e, I);
}
function IsTemporalZonedDateTime(e) {
  return HasSlot(e, n, g, p);
}
function RejectTemporalLikeObject(e) {
  if (HasSlot(e, p) || HasSlot(e, g)) throw new TypeError("with() does not support a calendar or timeZone property");
  if (IsTemporalTime(e)) throw new TypeError("with() does not accept Temporal.PlainTime, use withPlainTime() instead");
  if (void 0 !== e.calendar) throw new TypeError("with() does not support a calendar property");
  if (void 0 !== e.timeZone) throw new TypeError("with() does not support a timeZone property");
}
function ParseTemporalTimeZone(e) {
  var _ParseTemporalTimeZon = function ParseTemporalTimeZoneString(e) {
      if (new RegExp("^".concat($.source, "$"), "i").test(e)) return {
        ianaName: e
      };
      try {
        var _t3 = ParseISODateTime(e);
        if (_t3.z || _t3.offset || _t3.ianaName) return _t3;
      } catch (_unused) {}
      throw new RangeError("Invalid time zone: ".concat(e));
    }(e),
    t = _ParseTemporalTimeZon.ianaName,
    r = _ParseTemporalTimeZon.offset,
    o = _ParseTemporalTimeZon.z;
  if (t) return GetCanonicalTimeZoneIdentifier(t);
  if (o) return "UTC";
  return FormatTimeZoneOffsetString(ParseTimeZoneOffsetString(r));
}
function MaybeFormatCalendarAnnotation(e, t) {
  return "never" === t ? "" : FormatCalendarAnnotation(ToTemporalCalendarIdentifier(e), t);
}
function FormatCalendarAnnotation(e, t) {
  if ("never" === t) return "";
  if ("auto" === t && "iso8601" === e) return "";
  return "[".concat("critical" === t ? "!" : "", "u-ca=").concat(e, "]");
}
function ParseISODateTime(e) {
  var t = V.exec(e);
  if (!t) throw new RangeError("invalid ISO 8601 string: ".concat(e));
  var r = t[1];
  if ("−" === r[0] && (r = "-".concat(r.slice(1))), "-000000" === r) throw new RangeError("invalid ISO 8601 string: ".concat(e));
  var o = ToIntegerOrInfinity(r),
    n = ToIntegerOrInfinity(t[2] || t[4]),
    a = ToIntegerOrInfinity(t[3] || t[5]),
    i = ToIntegerOrInfinity(t[6]),
    s = void 0 !== t[6],
    l = ToIntegerOrInfinity(t[7] || t[10]);
  var d = ToIntegerOrInfinity(t[8] || t[11]);
  60 === d && (d = 59);
  var m = (t[9] || t[12]) + "000000000",
    c = ToIntegerOrInfinity(m.slice(0, 3)),
    h = ToIntegerOrInfinity(m.slice(3, 6)),
    u = ToIntegerOrInfinity(m.slice(6, 9));
  var T,
    p = !1;
  if (t[13]) T = void 0, p = !0;else if (t[14] && t[15]) {
    var _e2 = "-" === t[14] || "−" === t[14] ? "-" : "+",
      _r4 = t[15] || "00",
      _o = t[16] || "00",
      _n = t[17] || "00";
    var _a = t[18] || "0";
    if (T = "".concat(_e2).concat(_r4, ":").concat(_o), +_a) {
      for (; _a.endsWith("0");) _a = _a.slice(0, -1);
      T += ":".concat(_n, ".").concat(_a);
    } else +_n && (T += ":".concat(_n));
    "-00:00" === T && (T = "+00:00");
  }
  var f = t[19],
    y = t[20];
  var I;
  var _iterator4 = _createForOfIteratorHelper(y.matchAll(H)),
    _step4;
  try {
    for (_iterator4.s(); !(_step4 = _iterator4.n()).done;) {
      var _step4$value = _slicedToArray(_step4.value, 4),
        _e3 = _step4$value[1],
        _t4 = _step4$value[2],
        _r5 = _step4$value[3];
      if ("u-ca" === _t4) void 0 === I && (I = _r5);else if ("!" === _e3) throw new RangeError("Unrecognized annotation: !".concat(_t4, "=").concat(_r5));
    }
  } catch (err) {
    _iterator4.e(err);
  } finally {
    _iterator4.f();
  }
  return RejectDateTime(o, n, a, i, l, d, c, h, u), {
    year: o,
    month: n,
    day: a,
    hasTime: s,
    hour: i,
    minute: l,
    second: d,
    millisecond: c,
    microsecond: h,
    nanosecond: u,
    ianaName: f,
    offset: T,
    z: p,
    calendar: I
  };
}
function ParseTemporalYearMonthString(e) {
  var t = _.exec(e);
  var r, o, n, a;
  if (t) {
    var _a2 = t[1];
    if ("−" === _a2[0] && (_a2 = "-".concat(_a2.slice(1))), "-000000" === _a2) throw new RangeError("invalid ISO 8601 string: ".concat(e));
    r = ToIntegerOrInfinity(_a2), o = ToIntegerOrInfinity(t[2]);
    var _i = t[3];
    var _iterator5 = _createForOfIteratorHelper(_i.matchAll(H)),
      _step5;
    try {
      for (_iterator5.s(); !(_step5 = _iterator5.n()).done;) {
        var _step5$value = _slicedToArray(_step5.value, 4),
          _e4 = _step5$value[1],
          _t5 = _step5$value[2],
          _r6 = _step5$value[3];
        if ("u-ca" === _t5) void 0 === n && (n = _r6);else if ("!" === _e4) throw new RangeError("Unrecognized annotation: !".concat(_t5, "=").concat(_r6));
      }
    } catch (err) {
      _iterator5.e(err);
    } finally {
      _iterator5.f();
    }
    if (void 0 !== n && "iso8601" !== n) throw new RangeError("YYYY-MM format is only valid with iso8601 calendar");
  } else {
    var _ParseISODateTime;
    var _t6;
    if ((_ParseISODateTime = ParseISODateTime(e), r = _ParseISODateTime.year, o = _ParseISODateTime.month, n = _ParseISODateTime.calendar, a = _ParseISODateTime.day, _t6 = _ParseISODateTime.z), _t6) throw new RangeError("Z designator not supported for PlainYearMonth");
  }
  return {
    year: r,
    month: o,
    calendar: n,
    referenceISODay: a
  };
}
function ParseTemporalMonthDayString(e) {
  var t = J.exec(e);
  var r, o, n, a;
  if (t) {
    r = ToIntegerOrInfinity(t[1]), o = ToIntegerOrInfinity(t[2]);
    var _e5 = t[3];
    var _iterator6 = _createForOfIteratorHelper(_e5.matchAll(H)),
      _step6;
    try {
      for (_iterator6.s(); !(_step6 = _iterator6.n()).done;) {
        var _step6$value = _slicedToArray(_step6.value, 4),
          _t7 = _step6$value[1],
          _r7 = _step6$value[2],
          _o2 = _step6$value[3];
        if ("u-ca" === _r7) void 0 === n && (n = _o2);else if ("!" === _t7) throw new RangeError("Unrecognized annotation: !".concat(_r7, "=").concat(_o2));
      }
    } catch (err) {
      _iterator6.e(err);
    } finally {
      _iterator6.f();
    }
    if (void 0 !== n && "iso8601" !== n) throw new RangeError("MM-DD format is only valid with iso8601 calendar");
  } else {
    var _ParseISODateTime2;
    var _t8;
    if ((_ParseISODateTime2 = ParseISODateTime(e), r = _ParseISODateTime2.month, o = _ParseISODateTime2.day, n = _ParseISODateTime2.calendar, a = _ParseISODateTime2.year, _t8 = _ParseISODateTime2.z), _t8) throw new RangeError("Z designator not supported for PlainMonthDay");
  }
  return {
    month: r,
    day: o,
    calendar: n,
    referenceISOYear: a
  };
}
function ParseTemporalInstant(e) {
  var _ParseTemporalInstant = function ParseTemporalInstantString(e) {
      var t = ParseISODateTime(e);
      if (!t.z && !t.offset) throw new RangeError("Temporal.Instant requires a time zone offset");
      return t;
    }(e),
    t = _ParseTemporalInstant.year,
    r = _ParseTemporalInstant.month,
    o = _ParseTemporalInstant.day,
    n = _ParseTemporalInstant.hour,
    a = _ParseTemporalInstant.minute,
    i = _ParseTemporalInstant.second,
    s = _ParseTemporalInstant.millisecond,
    l = _ParseTemporalInstant.microsecond,
    d = _ParseTemporalInstant.nanosecond,
    m = _ParseTemporalInstant.offset,
    c = _ParseTemporalInstant.z;
  if (!c && !m) throw new RangeError("Temporal.Instant requires a time zone offset");
  var h = c ? 0 : ParseTimeZoneOffsetString(m);
  var _BalanceISODateTime = BalanceISODateTime(t, r, o, n, a, i, s, l, d - h);
  t = _BalanceISODateTime.year;
  r = _BalanceISODateTime.month;
  o = _BalanceISODateTime.day;
  n = _BalanceISODateTime.hour;
  a = _BalanceISODateTime.minute;
  i = _BalanceISODateTime.second;
  s = _BalanceISODateTime.millisecond;
  l = _BalanceISODateTime.microsecond;
  d = _BalanceISODateTime.nanosecond;
  var u = GetUTCEpochNanoseconds(t, r, o, n, a, i, s, l, d);
  if (null === u) throw new RangeError("DateTime outside of supported range");
  return u;
}
function RegulateISODate(e, t, r, o) {
  var n = e,
    a = t,
    i = r;
  switch (o) {
    case "reject":
      RejectISODate(n, a, i);
      break;
    case "constrain":
      var _ConstrainISODate = ConstrainISODate(n, a, i);
      n = _ConstrainISODate.year;
      a = _ConstrainISODate.month;
      i = _ConstrainISODate.day;
  }
  return {
    year: n,
    month: a,
    day: i
  };
}
function RegulateTime(e, t, r, o, n, a, i) {
  var s = e,
    l = t,
    d = r,
    m = o,
    c = n,
    h = a;
  switch (i) {
    case "reject":
      RejectTime(s, l, d, m, c, h);
      break;
    case "constrain":
      var _ConstrainTime = function ConstrainTime(e, t, r, o, n, a) {
        var i = ConstrainToRange(e, 0, 23),
          s = ConstrainToRange(t, 0, 59),
          l = ConstrainToRange(r, 0, 59),
          d = ConstrainToRange(o, 0, 999),
          m = ConstrainToRange(n, 0, 999),
          c = ConstrainToRange(a, 0, 999);
        return {
          hour: i,
          minute: s,
          second: l,
          millisecond: d,
          microsecond: m,
          nanosecond: c
        };
      }(s, l, d, m, c, h);
      s = _ConstrainTime.hour;
      l = _ConstrainTime.minute;
      d = _ConstrainTime.second;
      m = _ConstrainTime.millisecond;
      c = _ConstrainTime.microsecond;
      h = _ConstrainTime.nanosecond;
  }
  return {
    hour: s,
    minute: l,
    second: d,
    millisecond: m,
    microsecond: c,
    nanosecond: h
  };
}
function ToTemporalDurationRecord(e) {
  if (!IsObject(e)) return function ParseTemporalDurationString(e) {
    var t = Q.exec(e);
    if (!t) throw new RangeError("invalid duration: ".concat(e));
    if (t.slice(2).every(function (e) {
      return void 0 === e;
    })) throw new RangeError("invalid duration: ".concat(e));
    var r = "-" === t[1] || "−" === t[1] ? -1 : 1,
      o = void 0 === t[2] ? 0 : ToIntegerWithTruncation(t[2]) * r,
      n = void 0 === t[3] ? 0 : ToIntegerWithTruncation(t[3]) * r,
      a = void 0 === t[4] ? 0 : ToIntegerWithTruncation(t[4]) * r,
      i = void 0 === t[5] ? 0 : ToIntegerWithTruncation(t[5]) * r,
      s = void 0 === t[6] ? 0 : ToIntegerWithTruncation(t[6]) * r,
      l = t[7],
      d = t[8],
      m = t[9],
      c = t[10],
      h = t[11];
    var u = 0,
      T = 0,
      p = 0;
    if (void 0 !== l) {
      var _ref5, _ref6;
      if ((_ref5 = (_ref6 = d !== null && d !== void 0 ? d : m) !== null && _ref6 !== void 0 ? _ref6 : c) !== null && _ref5 !== void 0 ? _ref5 : h) throw new RangeError("only the smallest unit can be fractional");
      p = 3600 * ToIntegerOrInfinity((l + "000000000").slice(0, 9)) * r;
    } else if (u = void 0 === d ? 0 : ToIntegerWithTruncation(d) * r, void 0 !== m) {
      if (c !== null && c !== void 0 ? c : h) throw new RangeError("only the smallest unit can be fractional");
      p = 60 * ToIntegerOrInfinity((m + "000000000").slice(0, 9)) * r;
    } else T = void 0 === c ? 0 : ToIntegerWithTruncation(c) * r, void 0 !== h && (p = ToIntegerOrInfinity((h + "000000000").slice(0, 9)) * r);
    var f = p % 1e3,
      y = le(p / 1e3) % 1e3,
      I = le(p / 1e6) % 1e3;
    return T += le(p / 1e9) % 60, u += le(p / 6e10), RejectDuration(o, n, a, i, s, u, T, I, y, f), {
      years: o,
      months: n,
      weeks: a,
      days: i,
      hours: s,
      minutes: u,
      seconds: T,
      milliseconds: I,
      microseconds: y,
      nanoseconds: f
    };
  }(ToString(e));
  if (IsTemporalDuration(e)) return {
    years: GetSlot(e, w),
    months: GetSlot(e, D),
    weeks: GetSlot(e, G),
    days: GetSlot(e, v),
    hours: GetSlot(e, C),
    minutes: GetSlot(e, O),
    seconds: GetSlot(e, b),
    milliseconds: GetSlot(e, E),
    microseconds: GetSlot(e, M),
    nanoseconds: GetSlot(e, R)
  };
  var t = {
    years: 0,
    months: 0,
    weeks: 0,
    days: 0,
    hours: 0,
    minutes: 0,
    seconds: 0,
    milliseconds: 0,
    microseconds: 0,
    nanoseconds: 0
  };
  var r = function ToTemporalPartialDurationRecord(e) {
    if (!IsObject(e)) throw new TypeError("invalid duration-like");
    var t = {
      years: void 0,
      months: void 0,
      weeks: void 0,
      days: void 0,
      hours: void 0,
      minutes: void 0,
      seconds: void 0,
      milliseconds: void 0,
      microseconds: void 0,
      nanoseconds: void 0
    };
    var r = !1;
    var _iterator7 = _createForOfIteratorHelper(We),
      _step7;
    try {
      for (_iterator7.s(); !(_step7 = _iterator7.n()).done;) {
        var _o3 = _step7.value;
        var _n2 = e[_o3];
        void 0 !== _n2 && (r = !0, t[_o3] = ToIntegerIfIntegral(_n2));
      }
    } catch (err) {
      _iterator7.e(err);
    } finally {
      _iterator7.f();
    }
    if (!r) throw new TypeError("invalid duration-like");
    return t;
  }(e);
  var _iterator8 = _createForOfIteratorHelper(We),
    _step8;
  try {
    for (_iterator8.s(); !(_step8 = _iterator8.n()).done;) {
      var _e6 = _step8.value;
      var _o4 = r[_e6];
      void 0 !== _o4 && (t[_e6] = _o4);
    }
  } catch (err) {
    _iterator8.e(err);
  } finally {
    _iterator8.f();
  }
  var o = t.years,
    n = t.months,
    a = t.weeks,
    i = t.days,
    s = t.hours,
    l = t.minutes,
    d = t.seconds,
    m = t.milliseconds,
    c = t.microseconds,
    h = t.nanoseconds;
  return RejectDuration(o, n, a, i, s, l, d, m, c, h), {
    years: o,
    months: n,
    weeks: a,
    days: i,
    hours: s,
    minutes: l,
    seconds: d,
    milliseconds: m,
    microseconds: c,
    nanoseconds: h
  };
}
function ToTemporalOverflow(e) {
  return void 0 === e ? "constrain" : GetOption(e, "overflow", ["constrain", "reject"], "constrain");
}
function ToTemporalDisambiguation(e) {
  return void 0 === e ? "compatible" : GetOption(e, "disambiguation", ["compatible", "earlier", "later", "reject"], "compatible");
}
function ToTemporalRoundingMode(e, t) {
  return GetOption(e, "roundingMode", ["ceil", "floor", "expand", "trunc", "halfCeil", "halfFloor", "halfExpand", "halfTrunc", "halfEven"], t);
}
function ToTemporalOffset(e, t) {
  return void 0 === e ? t : GetOption(e, "offset", ["prefer", "use", "ignore", "reject"], t);
}
function ToCalendarNameOption(e) {
  return GetOption(e, "calendarName", ["auto", "always", "never", "critical"], "auto");
}
function ToTemporalRoundingIncrement(e) {
  var t = e.roundingIncrement;
  if (void 0 === t) return 1;
  if (t = ToNumber(t), !me(t)) throw new RangeError("roundingIncrement must be finite");
  var r = le(t);
  if (r < 1 || r > 1e9) throw new RangeError("roundingIncrement must be at least 1 and at most 1e9, not ".concat(t));
  return r;
}
function ValidateTemporalRoundingIncrement(e, t, r) {
  var o = r ? t : t - 1;
  if (e > o) throw new RangeError("roundingIncrement must be at least 1 and less than ".concat(o, ", not ").concat(e));
  if (t % e != 0) throw new RangeError("Rounding increment must divide evenly into ".concat(t));
}
function ToFractionalSecondDigits(e) {
  var t = e.fractionalSecondDigits;
  if (void 0 === t) return "auto";
  if ("number" != typeof t) {
    if ("auto" !== ToString(t)) throw new RangeError("fractionalSecondDigits must be 'auto' or 0 through 9, not ".concat(t));
    return "auto";
  }
  var r = ie(t);
  if (!me(r) || r < 0 || r > 9) throw new RangeError("fractionalSecondDigits must be 'auto' or 0 through 9, not ".concat(t));
  return r;
}
function ToSecondsStringPrecisionRecord(e, t) {
  switch (e) {
    case "minute":
      return {
        precision: "minute",
        unit: "minute",
        increment: 1
      };
    case "second":
      return {
        precision: 0,
        unit: "second",
        increment: 1
      };
    case "millisecond":
      return {
        precision: 3,
        unit: "millisecond",
        increment: 1
      };
    case "microsecond":
      return {
        precision: 6,
        unit: "microsecond",
        increment: 1
      };
    case "nanosecond":
      return {
        precision: 9,
        unit: "nanosecond",
        increment: 1
      };
  }
  switch (t) {
    case "auto":
      return {
        precision: t,
        unit: "nanosecond",
        increment: 1
      };
    case 0:
      return {
        precision: t,
        unit: "second",
        increment: 1
      };
    case 1:
    case 2:
    case 3:
      return {
        precision: t,
        unit: "millisecond",
        increment: Math.pow(10, 3 - t)
      };
    case 4:
    case 5:
    case 6:
      return {
        precision: t,
        unit: "microsecond",
        increment: Math.pow(10, 6 - t)
      };
    case 7:
    case 8:
    case 9:
      return {
        precision: t,
        unit: "nanosecond",
        increment: Math.pow(10, 9 - t)
      };
    default:
      throw new RangeError("fractionalSecondDigits must be 'auto' or 0 through 9, not ".concat(t));
  }
}
var He = Symbol("~required~");
function GetTemporalUnit(e, t, r, o) {
  var n = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : [];
  var a = [];
  for (var _i2 = 0, _Ue = Ue; _i2 < _Ue.length; _i2++) {
    var _Ue$_i = _slicedToArray(_Ue[_i2], 3),
      _e7 = _Ue$_i[1],
      _t9 = _Ue$_i[2];
    "datetime" !== r && r !== _t9 || a.push(_e7);
  }
  a.push.apply(a, _toConsumableArray(n));
  var i = o;
  i === He ? i = void 0 : void 0 !== i && a.push(i);
  var s = [].concat(a);
  for (var _i3 = 0, _a3 = a; _i3 < _a3.length; _i3++) {
    var _e8 = _a3[_i3];
    var _t10 = Le.get(_e8);
    void 0 !== _t10 && s.push(_t10);
  }
  var l = GetOption(e, t, s, i);
  if (void 0 === l && o === He) throw new RangeError("".concat(t, " is required"));
  return Ae.has(l) ? Ae.get(l) : l;
}
function ToRelativeTemporalObject(e) {
  var t = e.relativeTo;
  if (void 0 === t) return t;
  var r,
    o,
    n,
    a,
    i,
    s,
    l,
    d,
    m,
    c,
    h,
    u,
    T = "option",
    p = !1;
  if (IsObject(t)) {
    var _InterpretTemporalDat;
    if (IsTemporalZonedDateTime(t) || IsTemporalDate(t)) return t;
    if (IsTemporalDateTime(t)) return TemporalDateTimeToDate(t);
    c = GetTemporalCalendarSlotValueWithISODefault(t);
    var _e9 = CalendarFields(c, ["day", "hour", "microsecond", "millisecond", "minute", "month", "monthCode", "nanosecond", "second", "year"]);
    _e9.push("timeZone", "offset");
    var _p = PrepareTemporalFields(t, _e9, []),
      _f = Te(null);
    _f.overflow = "constrain", (_InterpretTemporalDat = InterpretTemporalDateTimeFields(c, _p, _f), r = _InterpretTemporalDat.year, o = _InterpretTemporalDat.month, n = _InterpretTemporalDat.day, a = _InterpretTemporalDat.hour, i = _InterpretTemporalDat.minute, s = _InterpretTemporalDat.second, l = _InterpretTemporalDat.millisecond, d = _InterpretTemporalDat.microsecond, m = _InterpretTemporalDat.nanosecond), u = _p.offset, void 0 === u && (T = "wall"), h = _p.timeZone, void 0 !== h && (h = ToTemporalTimeZoneSlotValue(h));
  } else {
    var _ParseISODateTime3;
    var _e10, _f2;
    if ((_ParseISODateTime3 = ParseISODateTime(ToString(t)), r = _ParseISODateTime3.year, o = _ParseISODateTime3.month, n = _ParseISODateTime3.day, a = _ParseISODateTime3.hour, i = _ParseISODateTime3.minute, s = _ParseISODateTime3.second, l = _ParseISODateTime3.millisecond, d = _ParseISODateTime3.microsecond, m = _ParseISODateTime3.nanosecond, c = _ParseISODateTime3.calendar, _e10 = _ParseISODateTime3.ianaName, u = _ParseISODateTime3.offset, _f2 = _ParseISODateTime3.z), _e10) h = ToTemporalTimeZoneSlotValue(_e10), _f2 ? T = "exact" : u || (T = "wall"), p = !0;else if (_f2) throw new RangeError("Z designator not supported for PlainDate relativeTo; either remove the Z or add a bracketed time zone");
    if (c || (c = "iso8601"), !IsBuiltinCalendar(c)) throw new RangeError("invalid calendar identifier ".concat(c));
    c = ASCIILowercase(c);
  }
  if (void 0 === h) return CreateTemporalDate(r, o, n, c);
  return CreateTemporalZonedDateTime(InterpretISODateTimeOffset(r, o, n, a, i, s, l, d, m, T, "option" === T ? ParseTimeZoneOffsetString(u) : 0, h, "compatible", "reject", p), h, c);
}
function DefaultTemporalLargestUnit(e, t, r, o, n, a, i, s, l, d) {
  for (var _i4 = 0, _arr = [["years", e], ["months", t], ["weeks", r], ["days", o], ["hours", n], ["minutes", a], ["seconds", i], ["milliseconds", s], ["microseconds", l], ["nanoseconds", d]]; _i4 < _arr.length; _i4++) {
    var _arr$_i = _slicedToArray(_arr[_i4], 2),
      _m = _arr$_i[0],
      _c = _arr$_i[1];
    if (0 !== _c) return Ae.get(_m);
  }
  return "nanosecond";
}
function LargerOfTwoTemporalUnits(e, t) {
  return xe.indexOf(e) > xe.indexOf(t) ? t : e;
}
function PrepareTemporalFields(e, t, r) {
  var _ref7 = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : {
      emptySourceErrorMessage: "no supported properties found"
    },
    o = _ref7.emptySourceErrorMessage;
  var n = Te(null);
  var a = !1;
  t.sort();
  var _iterator9 = _createForOfIteratorHelper(t),
    _step9;
  try {
    for (_iterator9.s(); !(_step9 = _iterator9.n()).done;) {
      var _o5 = _step9.value;
      var _t11 = e[_o5];
      if (void 0 !== _t11) a = !0, $e.has(_o5) && (_t11 = $e.get(_o5)(_t11)), n[_o5] = _t11;else if ("partial" !== r) {
        if (ee.call(r, _o5)) throw new TypeError("required property '".concat(_o5, "' missing or undefined"));
        _t11 = ke.get(_o5), n[_o5] = _t11;
      }
    }
  } catch (err) {
    _iterator9.e(err);
  } finally {
    _iterator9.f();
  }
  if ("partial" === r && !a) throw new TypeError(o);
  return n;
}
function ToTemporalTimeRecord(e) {
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "complete";
  var r = ["hour", "microsecond", "millisecond", "minute", "nanosecond", "second"],
    o = PrepareTemporalFields(e, r, "partial", {
      emptySourceErrorMessage: "invalid time-like"
    }),
    n = {};
  for (var _i5 = 0, _r8 = r; _i5 < _r8.length; _i5++) {
    var _e11 = _r8[_i5];
    var _r9 = pe(o, _e11);
    void 0 !== _r9 ? n[_e11] = _r9.value : "complete" === t && (n[_e11] = 0);
  }
  return n;
}
function ToTemporalDate(e, t) {
  var r = e;
  if (IsObject(r)) {
    if (IsTemporalDate(r)) return r;
    if (IsTemporalZonedDateTime(r) && (ToTemporalOverflow(t), r = GetPlainDateTimeFor(GetSlot(r, g), GetSlot(r, S), GetSlot(r, p))), IsTemporalDateTime(r)) return ToTemporalOverflow(t), CreateTemporalDate(GetSlot(r, i), GetSlot(r, s), GetSlot(r, l), GetSlot(r, p));
    var _e12 = GetTemporalCalendarSlotValueWithISODefault(r);
    return CalendarDateFromFields(_e12, PrepareTemporalFields(r, CalendarFields(_e12, ["day", "month", "monthCode", "year"]), []), t);
  }
  ToTemporalOverflow(t);
  var _ParseTemporalDateStr = function ParseTemporalDateString(e) {
      return ParseISODateTime(e);
    }(ToString(r)),
    o = _ParseTemporalDateStr.year,
    n = _ParseTemporalDateStr.month,
    a = _ParseTemporalDateStr.day,
    d = _ParseTemporalDateStr.calendar,
    m = _ParseTemporalDateStr.z;
  if (m) throw new RangeError("Z designator not supported for PlainDate");
  if (d || (d = "iso8601"), !IsBuiltinCalendar(d)) throw new RangeError("invalid calendar identifier ".concat(d));
  return d = ASCIILowercase(d), CreateTemporalDate(o, n, a, d);
}
function InterpretTemporalDateTimeFields(e, t, r) {
  var _RegulateTime;
  var _ToTemporalTimeRecord = ToTemporalTimeRecord(t),
    o = _ToTemporalTimeRecord.hour,
    n = _ToTemporalTimeRecord.minute,
    a = _ToTemporalTimeRecord.second,
    d = _ToTemporalTimeRecord.millisecond,
    m = _ToTemporalTimeRecord.microsecond,
    c = _ToTemporalTimeRecord.nanosecond;
  var h = ToTemporalOverflow(r),
    u = CalendarDateFromFields(e, t, r),
    T = GetSlot(u, i),
    p = GetSlot(u, s),
    f = GetSlot(u, l);
  return (_RegulateTime = RegulateTime(o, n, a, d, m, c, h), o = _RegulateTime.hour, n = _RegulateTime.minute, a = _RegulateTime.second, d = _RegulateTime.millisecond, m = _RegulateTime.microsecond, c = _RegulateTime.nanosecond), {
    year: T,
    month: p,
    day: f,
    hour: o,
    minute: n,
    second: a,
    millisecond: d,
    microsecond: m,
    nanosecond: c
  };
}
function ToTemporalDateTime(e, t) {
  var r, o, n, a, d, m, c, h, u, T;
  if (IsObject(e)) {
    if (IsTemporalDateTime(e)) return e;
    if (IsTemporalZonedDateTime(e)) return ToTemporalOverflow(t), GetPlainDateTimeFor(GetSlot(e, g), GetSlot(e, S), GetSlot(e, p));
    if (IsTemporalDate(e)) return ToTemporalOverflow(t), CreateTemporalDateTime(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l), 0, 0, 0, 0, 0, 0, GetSlot(e, p));
    T = GetTemporalCalendarSlotValueWithISODefault(e);
    var _f3 = PrepareTemporalFields(e, CalendarFields(T, ["day", "hour", "microsecond", "millisecond", "minute", "month", "monthCode", "nanosecond", "second", "year"]), []);
    var _InterpretTemporalDat2 = InterpretTemporalDateTimeFields(T, _f3, t);
    r = _InterpretTemporalDat2.year;
    o = _InterpretTemporalDat2.month;
    n = _InterpretTemporalDat2.day;
    a = _InterpretTemporalDat2.hour;
    d = _InterpretTemporalDat2.minute;
    m = _InterpretTemporalDat2.second;
    c = _InterpretTemporalDat2.millisecond;
    h = _InterpretTemporalDat2.microsecond;
    u = _InterpretTemporalDat2.nanosecond;
  } else {
    var _ParseTemporalDateTim;
    var _i6;
    if (ToTemporalOverflow(t), (_ParseTemporalDateTim = function ParseTemporalDateTimeString(e) {
      return ParseISODateTime(e);
    }(ToString(e)), r = _ParseTemporalDateTim.year, o = _ParseTemporalDateTim.month, n = _ParseTemporalDateTim.day, a = _ParseTemporalDateTim.hour, d = _ParseTemporalDateTim.minute, m = _ParseTemporalDateTim.second, c = _ParseTemporalDateTim.millisecond, h = _ParseTemporalDateTim.microsecond, u = _ParseTemporalDateTim.nanosecond, T = _ParseTemporalDateTim.calendar, _i6 = _ParseTemporalDateTim.z), _i6) throw new RangeError("Z designator not supported for PlainDateTime");
    if (RejectDateTime(r, o, n, a, d, m, c, h, u), T || (T = "iso8601"), !IsBuiltinCalendar(T)) throw new RangeError("invalid calendar identifier ".concat(T));
    T = ASCIILowercase(T);
  }
  return CreateTemporalDateTime(r, o, n, a, d, m, c, h, u, T);
}
function ToTemporalDuration(e) {
  if (IsTemporalDuration(e)) return e;
  var _ToTemporalDurationRe = ToTemporalDurationRecord(e),
    t = _ToTemporalDurationRe.years,
    r = _ToTemporalDurationRe.months,
    o = _ToTemporalDurationRe.weeks,
    n = _ToTemporalDurationRe.days,
    a = _ToTemporalDurationRe.hours,
    i = _ToTemporalDurationRe.minutes,
    s = _ToTemporalDurationRe.seconds,
    l = _ToTemporalDurationRe.milliseconds,
    d = _ToTemporalDurationRe.microseconds,
    m = _ToTemporalDurationRe.nanoseconds;
  return new (GetIntrinsic("%Temporal.Duration%"))(t, r, o, n, a, i, s, l, d, m);
}
function ToTemporalInstant(e) {
  if (IsTemporalInstant(e)) return e;
  if (IsTemporalZonedDateTime(e)) {
    return new (GetIntrinsic("%Temporal.Instant%"))(GetSlot(e, n));
  }
  var t = ParseTemporalInstant(ToString(e));
  return new (GetIntrinsic("%Temporal.Instant%"))(t);
}
function ToTemporalMonthDay(e, t) {
  var r = e;
  if (IsObject(r)) {
    if (IsTemporalMonthDay(r)) return r;
    var _e13, _o6;
    if (HasSlot(r, p)) _e13 = GetSlot(r, p), _o6 = !1;else {
      var _t12 = r.calendar;
      _o6 = void 0 === _t12, void 0 === _t12 && (_t12 = "iso8601"), _e13 = ToTemporalCalendarSlotValue(_t12);
    }
    var _n3 = PrepareTemporalFields(r, CalendarFields(_e13, ["day", "month", "monthCode", "year"]), []);
    return _o6 && void 0 !== _n3.month && void 0 === _n3.monthCode && void 0 === _n3.year && (_n3.year = 1972), CalendarMonthDayFromFields(_e13, _n3, t);
  }
  ToTemporalOverflow(t);
  var _ParseTemporalMonthDa = ParseTemporalMonthDayString(ToString(r)),
    o = _ParseTemporalMonthDa.month,
    n = _ParseTemporalMonthDa.day,
    a = _ParseTemporalMonthDa.referenceISOYear,
    i = _ParseTemporalMonthDa.calendar;
  if (void 0 === i && (i = "iso8601"), !IsBuiltinCalendar(i)) throw new RangeError("invalid calendar identifier ".concat(i));
  if (i = ASCIILowercase(i), void 0 === a) return RejectISODate(1972, o, n), CreateTemporalMonthDay(o, n, i);
  return CalendarMonthDayFromFields(i, CreateTemporalMonthDay(o, n, i, a));
}
function ToTemporalTime(e) {
  var _ParseTemporalTimeStr;
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "constrain";
  var r,
    o,
    n,
    a,
    i,
    s,
    l = e;
  if (IsObject(l)) {
    var _ToTemporalTimeRecord2, _RegulateTime2;
    if (IsTemporalTime(l)) return l;
    if (IsTemporalZonedDateTime(l) && (l = GetPlainDateTimeFor(GetSlot(l, g), GetSlot(l, S), GetSlot(l, p))), IsTemporalDateTime(l)) {
      return new (GetIntrinsic("%Temporal.PlainTime%"))(GetSlot(l, d), GetSlot(l, m), GetSlot(l, c), GetSlot(l, h), GetSlot(l, u), GetSlot(l, T));
    }
    (_ToTemporalTimeRecord2 = ToTemporalTimeRecord(l), r = _ToTemporalTimeRecord2.hour, o = _ToTemporalTimeRecord2.minute, n = _ToTemporalTimeRecord2.second, a = _ToTemporalTimeRecord2.millisecond, i = _ToTemporalTimeRecord2.microsecond, s = _ToTemporalTimeRecord2.nanosecond), (_RegulateTime2 = RegulateTime(r, o, n, a, i, s, t), r = _RegulateTime2.hour, o = _RegulateTime2.minute, n = _RegulateTime2.second, a = _RegulateTime2.millisecond, i = _RegulateTime2.microsecond, s = _RegulateTime2.nanosecond);
  } else (_ParseTemporalTimeStr = function ParseTemporalTimeString(e) {
    var t = z.exec(e);
    var r, o, n, a, i, s, l;
    if (t) {
      r = ToIntegerOrInfinity(t[1]), o = ToIntegerOrInfinity(t[2] || t[5]), n = ToIntegerOrInfinity(t[3] || t[6]), 60 === n && (n = 59);
      var _e14 = (t[4] || t[7]) + "000000000";
      a = ToIntegerOrInfinity(_e14.slice(0, 3)), i = ToIntegerOrInfinity(_e14.slice(3, 6)), s = ToIntegerOrInfinity(_e14.slice(6, 9)), l = t[14];
      var _iterator10 = _createForOfIteratorHelper(l.matchAll(H)),
        _step10;
      try {
        for (_iterator10.s(); !(_step10 = _iterator10.n()).done;) {
          var _step10$value = _slicedToArray(_step10.value, 4),
            _e15 = _step10$value[1],
            _t13 = _step10$value[2],
            _r10 = _step10$value[3];
          if ("u-ca" !== _t13 && "!" === _e15) throw new RangeError("Unrecognized annotation: !".concat(_t13, "=").concat(_r10));
        }
      } catch (err) {
        _iterator10.e(err);
      } finally {
        _iterator10.f();
      }
      if (t[8]) throw new RangeError("Z designator not supported for PlainTime");
    } else {
      var _ParseISODateTime4;
      var _t14, _l;
      if ((_ParseISODateTime4 = ParseISODateTime(e), _l = _ParseISODateTime4.hasTime, r = _ParseISODateTime4.hour, o = _ParseISODateTime4.minute, n = _ParseISODateTime4.second, a = _ParseISODateTime4.millisecond, i = _ParseISODateTime4.microsecond, s = _ParseISODateTime4.nanosecond, _t14 = _ParseISODateTime4.z), !_l) throw new RangeError("time is missing in string: ".concat(e));
      if (_t14) throw new RangeError("Z designator not supported for PlainTime");
    }
    if (/[tT ][0-9][0-9]/.test(e)) return {
      hour: r,
      minute: o,
      second: n,
      millisecond: a,
      microsecond: i,
      nanosecond: s
    };
    try {
      var _ParseTemporalMonthDa2 = ParseTemporalMonthDayString(e),
        _t15 = _ParseTemporalMonthDa2.month,
        _r11 = _ParseTemporalMonthDa2.day;
      RejectISODate(1972, _t15, _r11);
    } catch (_unused2) {
      try {
        var _ParseTemporalYearMon = ParseTemporalYearMonthString(e),
          _t16 = _ParseTemporalYearMon.year,
          _r12 = _ParseTemporalYearMon.month;
        RejectISODate(_t16, _r12, 1);
      } catch (_unused3) {
        return {
          hour: r,
          minute: o,
          second: n,
          millisecond: a,
          microsecond: i,
          nanosecond: s
        };
      }
    }
    throw new RangeError("invalid ISO 8601 time-only string ".concat(e, "; may need a T prefix"));
  }(ToString(l)), r = _ParseTemporalTimeStr.hour, o = _ParseTemporalTimeStr.minute, n = _ParseTemporalTimeStr.second, a = _ParseTemporalTimeStr.millisecond, i = _ParseTemporalTimeStr.microsecond, s = _ParseTemporalTimeStr.nanosecond), RejectTime(r, o, n, a, i, s);
  return new (GetIntrinsic("%Temporal.PlainTime%"))(r, o, n, a, i, s);
}
function ToTemporalYearMonth(e, t) {
  if (IsObject(e)) {
    if (IsTemporalYearMonth(e)) return e;
    var _r13 = GetTemporalCalendarSlotValueWithISODefault(e);
    return CalendarYearMonthFromFields(_r13, PrepareTemporalFields(e, CalendarFields(_r13, ["month", "monthCode", "year"]), []), t);
  }
  ToTemporalOverflow(t);
  var _ParseTemporalYearMon2 = ParseTemporalYearMonthString(ToString(e)),
    r = _ParseTemporalYearMon2.year,
    o = _ParseTemporalYearMon2.month,
    n = _ParseTemporalYearMon2.referenceISODay,
    a = _ParseTemporalYearMon2.calendar;
  if (void 0 === a && (a = "iso8601"), !IsBuiltinCalendar(a)) throw new RangeError("invalid calendar identifier ".concat(a));
  if (a = ASCIILowercase(a), void 0 === n) return RejectISODate(r, o, 1), CreateTemporalYearMonth(r, o, a);
  return CalendarYearMonthFromFields(a, CreateTemporalYearMonth(r, o, a, n));
}
function InterpretISODateTimeOffset(t, r, o, i, s, l, d, m, c, h, u, T, p, f, y) {
  var I = new (GetIntrinsic("%Temporal.PlainDateTime%"))(t, r, o, i, s, l, d, m, c);
  if ("wall" === h || "ignore" === f) {
    return GetSlot(GetInstantFor(T, I, p), n);
  }
  if ("exact" === h || "use" === f) {
    var _n4 = GetUTCEpochNanoseconds(t, r, o, i, s, l, d, m, c);
    if (null === _n4) throw new RangeError("ZonedDateTime outside of supported range");
    return _n4 - BigInt(u);
  }
  var S = GetPossibleInstantsFor(T, I);
  var _iterator11 = _createForOfIteratorHelper(S),
    _step11;
  try {
    for (_iterator11.s(); !(_step11 = _iterator11.n()).done;) {
      var _t18 = _step11.value;
      var _r14 = GetOffsetNanosecondsFor(T, _t18),
        _o7 = Number(RoundNumberToIncrement(BigInt(_r14), be, "halfExpand"));
      if (_r14 === u || y && _o7 === u) return GetSlot(_t18, n);
    }
  } catch (err) {
    _iterator11.e(err);
  } finally {
    _iterator11.f();
  }
  if ("reject" === f) {
    var _e16 = FormatTimeZoneOffsetString(u),
      _t17 = IsTemporalTimeZone(T) ? GetSlot(T, a) : "time zone";
    throw new RangeError("Offset ".concat(_e16, " is invalid for ").concat(I.toString(), " in ").concat(_t17));
  }
  return GetSlot(DisambiguatePossibleInstants(S, T, I, p), n);
}
function ToTemporalZonedDateTime(e, t) {
  var r,
    o,
    n,
    a,
    i,
    s,
    l,
    d,
    m,
    c,
    h,
    u,
    T,
    p,
    f = !1,
    y = "option";
  if (IsObject(e)) {
    var _InterpretTemporalDat3;
    if (IsTemporalZonedDateTime(e)) return e;
    u = GetTemporalCalendarSlotValueWithISODefault(e);
    var _f4 = CalendarFields(u, ["day", "hour", "microsecond", "millisecond", "minute", "month", "monthCode", "nanosecond", "second", "year"]);
    _f4.push("timeZone", "offset");
    var _I = PrepareTemporalFields(e, _f4, ["timeZone"]);
    c = ToTemporalTimeZoneSlotValue(_I.timeZone), h = _I.offset, void 0 === h && (y = "wall"), T = ToTemporalDisambiguation(t), p = ToTemporalOffset(t, "reject"), (_InterpretTemporalDat3 = InterpretTemporalDateTimeFields(u, _I, t), r = _InterpretTemporalDat3.year, o = _InterpretTemporalDat3.month, n = _InterpretTemporalDat3.day, a = _InterpretTemporalDat3.hour, i = _InterpretTemporalDat3.minute, s = _InterpretTemporalDat3.second, l = _InterpretTemporalDat3.millisecond, d = _InterpretTemporalDat3.microsecond, m = _InterpretTemporalDat3.nanosecond);
  } else {
    var _ParseTemporalZonedDa;
    var _I2, _S;
    if ((_ParseTemporalZonedDa = function ParseTemporalZonedDateTimeString(e) {
      var t = ParseISODateTime(e);
      if (!t.ianaName) throw new RangeError("Temporal.ZonedDateTime requires a time zone ID in brackets");
      return t;
    }(ToString(e)), r = _ParseTemporalZonedDa.year, o = _ParseTemporalZonedDa.month, n = _ParseTemporalZonedDa.day, a = _ParseTemporalZonedDa.hour, i = _ParseTemporalZonedDa.minute, s = _ParseTemporalZonedDa.second, l = _ParseTemporalZonedDa.millisecond, d = _ParseTemporalZonedDa.microsecond, m = _ParseTemporalZonedDa.nanosecond, _I2 = _ParseTemporalZonedDa.ianaName, h = _ParseTemporalZonedDa.offset, _S = _ParseTemporalZonedDa.z, u = _ParseTemporalZonedDa.calendar), c = ToTemporalTimeZoneSlotValue(_I2), _S ? y = "exact" : h || (y = "wall"), u || (u = "iso8601"), !IsBuiltinCalendar(u)) throw new RangeError("invalid calendar identifier ".concat(u));
    u = ASCIILowercase(u), f = !0, T = ToTemporalDisambiguation(t), p = ToTemporalOffset(t, "reject"), ToTemporalOverflow(t);
  }
  var I = 0;
  "option" === y && (I = ParseTimeZoneOffsetString(h));
  return CreateTemporalZonedDateTime(InterpretISODateTimeOffset(r, o, n, a, i, s, l, d, m, y, I, c, T, p, f), c, u);
}
function CreateTemporalDateSlots(e, t, r, o, n) {
  RejectISODate(t, r, o), RejectDateRange(t, r, o), N(e), SetSlot(e, i, t), SetSlot(e, s, r), SetSlot(e, l, o), SetSlot(e, p, n), SetSlot(e, f, !0);
}
function CreateTemporalDate(e, t, r) {
  var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : "iso8601";
  var n = GetIntrinsic("%Temporal.PlainDate%"),
    a = Te(n.prototype);
  return CreateTemporalDateSlots(a, e, t, r, o), a;
}
function CreateTemporalDateTimeSlots(e, t, r, o, n, a, f, y, I, S, g) {
  RejectDateTime(t, r, o, n, a, f, y, I, S), RejectDateTimeRange(t, r, o, n, a, f, y, I, S), N(e), SetSlot(e, i, t), SetSlot(e, s, r), SetSlot(e, l, o), SetSlot(e, d, n), SetSlot(e, m, a), SetSlot(e, c, f), SetSlot(e, h, y), SetSlot(e, u, I), SetSlot(e, T, S), SetSlot(e, p, g);
}
function CreateTemporalDateTime(e, t, r, o, n, a, i, s, l) {
  var d = arguments.length > 9 && arguments[9] !== undefined ? arguments[9] : "iso8601";
  var m = GetIntrinsic("%Temporal.PlainDateTime%"),
    c = Te(m.prototype);
  return CreateTemporalDateTimeSlots(c, e, t, r, o, n, a, i, s, l, d), c;
}
function CreateTemporalMonthDaySlots(e, t, r, o, n) {
  RejectISODate(n, t, r), RejectDateRange(n, t, r), N(e), SetSlot(e, s, t), SetSlot(e, l, r), SetSlot(e, i, n), SetSlot(e, p, o), SetSlot(e, I, !0);
}
function CreateTemporalMonthDay(e, t) {
  var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "iso8601";
  var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1972;
  var n = GetIntrinsic("%Temporal.PlainMonthDay%"),
    a = Te(n.prototype);
  return CreateTemporalMonthDaySlots(a, e, t, r, o), a;
}
function CreateTemporalYearMonthSlots(e, t, r, o, n) {
  RejectISODate(t, r, n), function RejectYearMonthRange(e, t) {
    RejectToRange(e, Fe, Ye), e === Fe ? RejectToRange(t, 4, 12) : e === Ye && RejectToRange(t, 1, 9);
  }(t, r), N(e), SetSlot(e, i, t), SetSlot(e, s, r), SetSlot(e, l, n), SetSlot(e, p, o), SetSlot(e, y, !0);
}
function CreateTemporalYearMonth(e, t) {
  var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "iso8601";
  var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1;
  var n = GetIntrinsic("%Temporal.PlainYearMonth%"),
    a = Te(n.prototype);
  return CreateTemporalYearMonthSlots(a, e, t, r, o), a;
}
function CreateTemporalZonedDateTimeSlots(e, t, r, o) {
  ValidateEpochNanoseconds(t), N(e), SetSlot(e, n, t), SetSlot(e, g, r), SetSlot(e, p, o);
  var a = new (GetIntrinsic("%Temporal.Instant%"))(GetSlot(e, n));
  SetSlot(e, S, a);
}
function CreateTemporalZonedDateTime(e, t) {
  var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "iso8601";
  var o = GetIntrinsic("%Temporal.ZonedDateTime%"),
    n = Te(o.prototype);
  return CreateTemporalZonedDateTimeSlots(n, e, t, r), n;
}
function CalendarFields(e, t) {
  if ("string" == typeof e) {
    var _r15 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.fields%"), _r15, [t]);
  }
  var r = Call(GetMethod(e, "fields"), e, [t]),
    o = [];
  var _iterator12 = _createForOfIteratorHelper(r),
    _step12;
  try {
    for (_iterator12.s(); !(_step12 = _iterator12.n()).done;) {
      var _e17 = _step12.value;
      if ("string" != typeof _e17) throw new TypeError("bad return from calendar.fields()");
      te.call(o, _e17);
    }
  } catch (err) {
    _iterator12.e(err);
  } finally {
    _iterator12.f();
  }
  return o;
}
function CalendarMergeFields(e, t, r) {
  if ("string" == typeof e) {
    var _o8 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.mergeFields%"), _o8, [t, r]);
  }
  var o = Call(GetMethod(e, "mergeFields"), e, [t, r]);
  if (!IsObject(o)) throw new TypeError("bad return from calendar.mergeFields()");
  return o;
}
function CalendarDateAdd(e, t, r, o, n) {
  var a = n;
  if ("string" == typeof e) {
    var _n5 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.dateAdd%"), _n5, [t, r, o]);
  }
  void 0 === a && (a = GetMethod(e, "dateAdd"));
  var i = fe(a, e, [t, r, o]);
  if (!IsTemporalDate(i)) throw new TypeError("invalid result");
  return i;
}
function CalendarDateUntil(e, t, r, o, n) {
  var a = n;
  if ("string" == typeof e) {
    var _n6 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.dateUntil%"), _n6, [t, r, o]);
  }
  void 0 === a && (a = GetMethod(e, "dateUntil"));
  var i = fe(a, e, [t, r, o]);
  if (!IsTemporalDuration(i)) throw new TypeError("invalid result");
  return i;
}
function CalendarYear(e, t) {
  if ("string" == typeof e) {
    var _r16 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.year%"), _r16, [t]);
  }
  var r = Call(GetMethod(e, "year"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar year result must be an integer");
  if (!IsIntegralNumber(r)) throw new RangeError("calendar year result must be an integer");
  return r;
}
function CalendarMonth(e, t) {
  if ("string" == typeof e) {
    var _r17 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.month%"), _r17, [t]);
  }
  var r = Call(GetMethod(e, "month"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar month result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar month result must be a positive integer");
  return r;
}
function CalendarMonthCode(e, t) {
  if ("string" == typeof e) {
    var _r18 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.monthCode%"), _r18, [t]);
  }
  var r = Call(GetMethod(e, "monthCode"), e, [t]);
  if ("string" != typeof r) throw new TypeError("calendar monthCode result must be a string");
  return r;
}
function CalendarDay(e, t) {
  if ("string" == typeof e) {
    var _r19 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.day%"), _r19, [t]);
  }
  var r = Call(GetMethod(e, "day"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar day result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar day result must be a positive integer");
  return r;
}
function CalendarEra(e, t) {
  if ("string" == typeof e) {
    var _r20 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.era%"), _r20, [t]);
  }
  var r = Call(GetMethod(e, "era"), e, [t]);
  if (void 0 === r) return r;
  if ("string" != typeof r) throw new TypeError("calendar era result must be a string or undefined");
  return r;
}
function CalendarEraYear(e, t) {
  if ("string" == typeof e) {
    var _r21 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.eraYear%"), _r21, [t]);
  }
  var r = Call(GetMethod(e, "eraYear"), e, [t]);
  if (void 0 === r) return r;
  if ("number" != typeof r) throw new TypeError("calendar eraYear result must be an integer or undefined");
  if (!IsIntegralNumber(r)) throw new RangeError("calendar eraYear result must be an integer or undefined");
  return r;
}
function CalendarDayOfWeek(e, t) {
  if ("string" == typeof e) {
    var _r22 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.dayOfWeek%"), _r22, [t]);
  }
  var r = Call(GetMethod(e, "dayOfWeek"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar dayOfWeek result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar dayOfWeek result must be a positive integer");
  return r;
}
function CalendarDayOfYear(e, t) {
  if ("string" == typeof e) {
    var _r23 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.dayOfYear%"), _r23, [t]);
  }
  var r = Call(GetMethod(e, "dayOfYear"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar dayOfYear result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar dayOfYear result must be a positive integer");
  return r;
}
function CalendarWeekOfYear(e, t) {
  if ("string" == typeof e) {
    var _r24 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.weekOfYear%"), _r24, [t]);
  }
  var r = Call(GetMethod(e, "weekOfYear"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar weekOfYear result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar weekOfYear result must be a positive integer");
  return r;
}
function CalendarYearOfWeek(e, t) {
  if ("string" == typeof e) {
    var _r25 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.yearOfWeek%"), _r25, [t]);
  }
  var r = Call(GetMethod(e, "yearOfWeek"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar yearOfWeek result must be an integer");
  if (!IsIntegralNumber(r)) throw new RangeError("calendar yearOfWeek result must be an integer");
  return r;
}
function CalendarDaysInWeek(e, t) {
  if ("string" == typeof e) {
    var _r26 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.daysInWeek%"), _r26, [t]);
  }
  var r = Call(GetMethod(e, "daysInWeek"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar daysInWeek result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar daysInWeek result must be a positive integer");
  return r;
}
function CalendarDaysInMonth(e, t) {
  if ("string" == typeof e) {
    var _r27 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.daysInMonth%"), _r27, [t]);
  }
  var r = Call(GetMethod(e, "daysInMonth"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar daysInMonth result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar daysInMonth result must be a positive integer");
  return r;
}
function CalendarDaysInYear(e, t) {
  if ("string" == typeof e) {
    var _r28 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.daysInYear%"), _r28, [t]);
  }
  var r = Call(GetMethod(e, "daysInYear"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar daysInYear result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar daysInYear result must be a positive integer");
  return r;
}
function CalendarMonthsInYear(e, t) {
  if ("string" == typeof e) {
    var _r29 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.monthsInYear%"), _r29, [t]);
  }
  var r = Call(GetMethod(e, "monthsInYear"), e, [t]);
  if ("number" != typeof r) throw new TypeError("calendar monthsInYear result must be a positive integer");
  if (!IsIntegralNumber(r) || r < 1) throw new RangeError("calendar monthsInYear result must be a positive integer");
  return r;
}
function CalendarInLeapYear(e, t) {
  if ("string" == typeof e) {
    var _r30 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.inLeapYear%"), _r30, [t]);
  }
  var r = Call(GetMethod(e, "inLeapYear"), e, [t]);
  if ("boolean" != typeof r) throw new TypeError("calendar inLeapYear result must be a boolean");
  return r;
}
function ToTemporalCalendarSlotValue(e) {
  if (IsObject(e)) {
    if (HasSlot(e, p)) return GetSlot(e, p);
    if (!function ObjectImplementsTemporalCalendarProtocol(e) {
      return !!IsTemporalCalendar(e) || "dateAdd" in e && "dateFromFields" in e && "dateUntil" in e && "day" in e && "dayOfWeek" in e && "dayOfYear" in e && "daysInMonth" in e && "daysInWeek" in e && "daysInYear" in e && "fields" in e && "id" in e && "inLeapYear" in e && "mergeFields" in e && "month" in e && "monthCode" in e && "monthDayFromFields" in e && "monthsInYear" in e && "weekOfYear" in e && "year" in e && "yearMonthFromFields" in e && "yearOfWeek" in e;
    }(e)) throw new TypeError("expected a Temporal.Calendar or object implementing the Temporal.Calendar protocol");
    return e;
  }
  var t = ToString(e);
  if (IsBuiltinCalendar(t)) return ASCIILowercase(t);
  var r;
  try {
    var _ParseISODateTime5 = ParseISODateTime(t);
    r = _ParseISODateTime5.calendar;
  } catch (_unused4) {
    try {
      var _ParseTemporalYearMon3 = ParseTemporalYearMonthString(t);
      r = _ParseTemporalYearMon3.calendar;
    } catch (_unused5) {
      var _ParseTemporalMonthDa3 = ParseTemporalMonthDayString(t);
      r = _ParseTemporalMonthDa3.calendar;
    }
  }
  if (r || (r = "iso8601"), !IsBuiltinCalendar(r)) throw new RangeError("invalid calendar identifier ".concat(r));
  return ASCIILowercase(r);
}
function GetTemporalCalendarSlotValueWithISODefault(e) {
  if (HasSlot(e, p)) return GetSlot(e, p);
  var t = e.calendar;
  return void 0 === t ? "iso8601" : ToTemporalCalendarSlotValue(t);
}
function ToTemporalCalendarIdentifier(e) {
  if ("string" == typeof e) return e;
  var t = e.id;
  if ("string" != typeof t) throw new TypeError("calendar.id should be a string");
  return t;
}
function ToTemporalCalendarObject(e) {
  if (IsObject(e)) return e;
  return new (GetIntrinsic("%Temporal.Calendar%"))(e);
}
function CalendarEquals(e, t) {
  if (e === t) return !0;
  return ToTemporalCalendarIdentifier(e) === ToTemporalCalendarIdentifier(t);
}
function ThrowIfCalendarsNotEqual(e, t, r) {
  if (e === t) return;
  var o = ToTemporalCalendarIdentifier(e),
    n = ToTemporalCalendarIdentifier(t);
  if (o !== n) throw new RangeError("cannot ".concat(r, " of ").concat(o, " and ").concat(n, " calendars"));
}
function ConsolidateCalendars(e, t) {
  if (e === t) return t;
  var r = ToTemporalCalendarIdentifier(e),
    o = ToTemporalCalendarIdentifier(t);
  if (r === o || "iso8601" === r) return t;
  if ("iso8601" === o) return e;
  throw new RangeError("irreconcilable calendars");
}
function CalendarDateFromFields(e, t, r, o) {
  if ("string" == typeof e) {
    var _o9 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.dateFromFields%"), _o9, [t, r]);
  }
  var n = Call(o !== null && o !== void 0 ? o : GetMethod(e, "dateFromFields"), e, [t, r]);
  if (!IsTemporalDate(n)) throw new TypeError("invalid result");
  return n;
}
function CalendarYearMonthFromFields(e, t, r) {
  if ("string" == typeof e) {
    var _o10 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.yearMonthFromFields%"), _o10, [t, r]);
  }
  var o = Call(GetMethod(e, "yearMonthFromFields"), e, [t, r]);
  if (!IsTemporalYearMonth(o)) throw new TypeError("invalid result");
  return o;
}
function CalendarMonthDayFromFields(e, t, r) {
  if ("string" == typeof e) {
    var _o11 = new (GetIntrinsic("%Temporal.Calendar%"))(e);
    return Call(GetIntrinsic("%Temporal.Calendar.prototype.monthDayFromFields%"), _o11, [t, r]);
  }
  var o = Call(GetMethod(e, "monthDayFromFields"), e, [t, r]);
  if (!IsTemporalMonthDay(o)) throw new TypeError("invalid result");
  return o;
}
function ToTemporalTimeZoneSlotValue(e) {
  if (IsObject(e)) {
    if (IsTemporalZonedDateTime(e)) return GetSlot(e, g);
    if (!function ObjectImplementsTemporalTimeZoneProtocol(e) {
      return !!IsTemporalTimeZone(e) || "getOffsetNanosecondsFor" in e && "getPossibleInstantsFor" in e && "id" in e;
    }(e)) throw new TypeError("expected a Temporal.TimeZone or object implementing the Temporal.TimeZone protocol");
    return e;
  }
  return ParseTemporalTimeZone(ToString(e));
}
function ToTemporalTimeZoneIdentifier(e) {
  if ("string" == typeof e) return e;
  var t = e.id;
  if ("string" != typeof t) throw new TypeError("timeZone.id should be a string");
  return t;
}
function ToTemporalTimeZoneObject(e) {
  if (IsObject(e)) return e;
  return new (GetIntrinsic("%Temporal.TimeZone%"))(e);
}
function TimeZoneEquals(e, t) {
  if (e === t) return !0;
  return ToTemporalTimeZoneIdentifier(e) === ToTemporalTimeZoneIdentifier(t);
}
function TemporalDateTimeToDate(e) {
  return CreateTemporalDate(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l), GetSlot(e, p));
}
function TemporalDateTimeToTime(e) {
  return new (GetIntrinsic("%Temporal.PlainTime%"))(GetSlot(e, d), GetSlot(e, m), GetSlot(e, c), GetSlot(e, h), GetSlot(e, u), GetSlot(e, T));
}
function GetOffsetNanosecondsFor(e, t, r) {
  if ("string" == typeof e) {
    var _r31 = new (GetIntrinsic("%Temporal.TimeZone%"))(e);
    return Call(GetIntrinsic("%Temporal.TimeZone.prototype.getOffsetNanosecondsFor%"), _r31, [t]);
  }
  var o = Call(r !== null && r !== void 0 ? r : GetMethod(e, "getOffsetNanosecondsFor"), e, [t]);
  if ("number" != typeof o) throw new TypeError("bad return from getOffsetNanosecondsFor");
  if (!IsIntegralNumber(o) || ae(o) >= 864e11) throw new RangeError("out-of-range return from getOffsetNanosecondsFor");
  return o;
}
function GetOffsetStringFor(e, t) {
  return FormatTimeZoneOffsetString(GetOffsetNanosecondsFor(e, t));
}
function GetPlainDateTimeFor(e, t, r) {
  var _BalanceISODateTime2;
  var o = GetSlot(t, n),
    a = GetOffsetNanosecondsFor(e, t);
  var _GetISOPartsFromEpoch = GetISOPartsFromEpoch(o),
    i = _GetISOPartsFromEpoch.year,
    s = _GetISOPartsFromEpoch.month,
    l = _GetISOPartsFromEpoch.day,
    d = _GetISOPartsFromEpoch.hour,
    m = _GetISOPartsFromEpoch.minute,
    c = _GetISOPartsFromEpoch.second,
    h = _GetISOPartsFromEpoch.millisecond,
    u = _GetISOPartsFromEpoch.microsecond,
    T = _GetISOPartsFromEpoch.nanosecond;
  return (_BalanceISODateTime2 = BalanceISODateTime(i, s, l, d, m, c, h, u, T + a), i = _BalanceISODateTime2.year, s = _BalanceISODateTime2.month, l = _BalanceISODateTime2.day, d = _BalanceISODateTime2.hour, m = _BalanceISODateTime2.minute, c = _BalanceISODateTime2.second, h = _BalanceISODateTime2.millisecond, u = _BalanceISODateTime2.microsecond, T = _BalanceISODateTime2.nanosecond), CreateTemporalDateTime(i, s, l, d, m, c, h, u, T, r);
}
function GetInstantFor(e, t, r) {
  return DisambiguatePossibleInstants(GetPossibleInstantsFor(e, t), e, t, r);
}
function DisambiguatePossibleInstants(t, r, o, n) {
  var a = GetIntrinsic("%Temporal.Instant%"),
    f = t.length;
  if (1 === f) return t[0];
  if (f) switch (n) {
    case "compatible":
    case "earlier":
      return t[0];
    case "later":
      return t[f - 1];
    case "reject":
      throw new RangeError("multiple instants found");
  }
  var y = GetSlot(o, i),
    I = GetSlot(o, s),
    S = GetSlot(o, l),
    g = GetSlot(o, d),
    w = GetSlot(o, m),
    D = GetSlot(o, c),
    G = GetSlot(o, h),
    v = GetSlot(o, u),
    C = GetSlot(o, T),
    O = GetUTCEpochNanoseconds(y, I, S, g, w, D, G, v, C);
  if (null === O) throw new RangeError("DateTime outside of supported range");
  var b = new a(O - Ee),
    E = new a(O + Ee),
    M = GetOffsetNanosecondsFor(r, b),
    R = GetOffsetNanosecondsFor(r, E) - M;
  switch (n) {
    case "earlier":
      {
        var _e18 = GetSlot(o, p),
          _t19 = GetIntrinsic("%Temporal.PlainDateTime%"),
          _n7 = AddDateTime(y, I, S, g, w, D, G, v, C, _e18, 0, 0, 0, 0, 0, 0, 0, 0, 0, -R, void 0);
        return GetPossibleInstantsFor(r, new _t19(_n7.year, _n7.month, _n7.day, _n7.hour, _n7.minute, _n7.second, _n7.millisecond, _n7.microsecond, _n7.nanosecond, _e18))[0];
      }
    case "compatible":
    case "later":
      {
        var _e19 = GetSlot(o, p),
          _t20 = GetIntrinsic("%Temporal.PlainDateTime%"),
          _n8 = AddDateTime(y, I, S, g, w, D, G, v, C, _e19, 0, 0, 0, 0, 0, 0, 0, 0, 0, R, void 0),
          _a4 = GetPossibleInstantsFor(r, new _t20(_n8.year, _n8.month, _n8.day, _n8.hour, _n8.minute, _n8.second, _n8.millisecond, _n8.microsecond, _n8.nanosecond, _e19));
        return _a4[_a4.length - 1];
      }
    case "reject":
      throw new RangeError("no such instant found");
  }
}
function GetPossibleInstantsFor(e, t, r) {
  if ("string" == typeof e) {
    var _r32 = new (GetIntrinsic("%Temporal.TimeZone%"))(e);
    return Call(GetIntrinsic("%Temporal.TimeZone.prototype.getPossibleInstantsFor%"), _r32, [t]);
  }
  var o = Call(r !== null && r !== void 0 ? r : GetMethod(e, "getPossibleInstantsFor"), e, [t]),
    n = [];
  var _iterator13 = _createForOfIteratorHelper(o),
    _step13;
  try {
    for (_iterator13.s(); !(_step13 = _iterator13.n()).done;) {
      var _e20 = _step13.value;
      if (!IsTemporalInstant(_e20)) throw new TypeError("bad return from getPossibleInstantsFor");
      te.call(n, _e20);
    }
  } catch (err) {
    _iterator13.e(err);
  } finally {
    _iterator13.f();
  }
  return n;
}
function ISOYearString(e) {
  var t;
  if (e < 0 || e > 9999) {
    t = (e < 0 ? "-" : "+") + "000000".concat(ae(e)).slice(-6);
  } else t = "0000".concat(e).slice(-4);
  return t;
}
function ISODateTimePartString(e) {
  return "00".concat(e).slice(-2);
}
function FormatSecondsStringPart(e, t, r, o, n) {
  if ("minute" === n) return "";
  var a = ":".concat(ISODateTimePartString(e));
  var i,
    s = 1e6 * t + 1e3 * r + o;
  if ("auto" === n) {
    if (0 === s) return a;
    for (i = "".concat(s).padStart(9, "0"); "0" === i[i.length - 1];) i = i.slice(0, -1);
  } else {
    if (0 === n) return a;
    i = "".concat(s).padStart(9, "0").slice(0, n);
  }
  return "".concat(a, ".").concat(i);
}
function TemporalInstantToString(e, t, r) {
  var o = t;
  void 0 === o && (o = "UTC");
  var n = GetPlainDateTimeFor(o, e, "iso8601"),
    a = ISOYearString(GetSlot(n, i)),
    p = ISODateTimePartString(GetSlot(n, s)),
    f = ISODateTimePartString(GetSlot(n, l)),
    y = ISODateTimePartString(GetSlot(n, d)),
    I = ISODateTimePartString(GetSlot(n, m)),
    S = FormatSecondsStringPart(GetSlot(n, c), GetSlot(n, h), GetSlot(n, u), GetSlot(n, T), r);
  var g = "Z";
  if (void 0 !== t) {
    g = FormatISOTimeZoneOffsetString(GetOffsetNanosecondsFor(o, e));
  }
  return "".concat(a, "-").concat(p, "-").concat(f, "T").concat(y, ":").concat(I).concat(S).concat(g);
}
function TemporalDurationToString(t) {
  var _divmod, _divmod2, _divmod3;
  var r = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "auto";
  var o = arguments.length > 2 ? arguments[2] : undefined;
  function formatNumber(t) {
    return t <= ue ? t.toString(10) : BigInt(t).toString(10);
  }
  var n = GetSlot(t, w),
    a = GetSlot(t, D),
    i = GetSlot(t, G),
    s = GetSlot(t, v),
    l = GetSlot(t, C),
    d = GetSlot(t, O);
  var m = GetSlot(t, b),
    c = GetSlot(t, E),
    h = GetSlot(t, M),
    u = GetSlot(t, R);
  var T = DurationSign(n, a, i, s, l, d, m, c, h, u);
  if (o) {
    var _e21 = o.unit,
      _t21 = o.increment,
      _r33 = o.roundingMode;
    var _RoundDuration = RoundDuration(0, 0, 0, 0, 0, 0, m, c, h, u, _t21, _e21, _r33);
    m = _RoundDuration.seconds;
    c = _RoundDuration.milliseconds;
    h = _RoundDuration.microseconds;
    u = _RoundDuration.nanoseconds;
  }
  var p = [];
  n && p.push("".concat(formatNumber(ae(n)), "Y")), a && p.push("".concat(formatNumber(ae(a)), "M")), i && p.push("".concat(formatNumber(ae(i)), "W")), s && p.push("".concat(formatNumber(ae(s)), "D"));
  var f = [];
  l && f.push("".concat(formatNumber(ae(l)), "H")), d && f.push("".concat(formatNumber(ae(d)), "M"));
  var y = [];
  var I,
    S,
    g,
    F,
    Y = TotalDurationNanoseconds(0, 0, 0, m, c, h, u, 0);
  (_divmod = divmod(Y, De), Y = _divmod.quotient, I = _divmod.remainder), (_divmod2 = divmod(Y, De), Y = _divmod2.quotient, S = _divmod2.remainder), (_divmod3 = divmod(Y, De), F = _divmod3.quotient, g = _divmod3.remainder);
  var P = 1e6 * ae(Number(g)) + 1e3 * ae(Number(S)) + ae(Number(I));
  var Z;
  if ("auto" === r) {
    if (0 !== P) for (Z = "".concat(P).padStart(9, "0"); "0" === Z[Z.length - 1];) Z = Z.slice(0, -1);
  } else 0 !== r && (Z = "".concat(P).padStart(9, "0").slice(0, r));
  return Z && y.unshift(".", Z), F === Ie && !y.length && "auto" === r || y.unshift(abs(F).toString()), y.length && f.push("".concat(y.join(""), "S")), f.length && f.unshift("T"), p.length || f.length ? "".concat(T < 0 ? "-" : "", "P").concat(p.join("")).concat(f.join("")) : "PT0S";
}
function TemporalDateToString(e) {
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "auto";
  return "".concat(ISOYearString(GetSlot(e, i)), "-").concat(ISODateTimePartString(GetSlot(e, s)), "-").concat(ISODateTimePartString(GetSlot(e, l))).concat(MaybeFormatCalendarAnnotation(GetSlot(e, p), t));
}
function TemporalDateTimeToString(e, t) {
  var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "auto";
  var o = arguments.length > 3 ? arguments[3] : undefined;
  var n = GetSlot(e, i),
    a = GetSlot(e, s),
    f = GetSlot(e, l),
    y = GetSlot(e, d),
    I = GetSlot(e, m),
    S = GetSlot(e, c),
    g = GetSlot(e, h),
    w = GetSlot(e, u),
    D = GetSlot(e, T);
  if (o) {
    var _e22 = o.unit,
      _t22 = o.increment,
      _r34 = o.roundingMode;
    var _RoundISODateTime = RoundISODateTime(n, a, f, y, I, S, g, w, D, _t22, _e22, _r34);
    n = _RoundISODateTime.year;
    a = _RoundISODateTime.month;
    f = _RoundISODateTime.day;
    y = _RoundISODateTime.hour;
    I = _RoundISODateTime.minute;
    S = _RoundISODateTime.second;
    g = _RoundISODateTime.millisecond;
    w = _RoundISODateTime.microsecond;
    D = _RoundISODateTime.nanosecond;
  }
  return "".concat(ISOYearString(n), "-").concat(ISODateTimePartString(a), "-").concat(ISODateTimePartString(f), "T").concat(ISODateTimePartString(y), ":").concat(ISODateTimePartString(I)).concat(FormatSecondsStringPart(S, g, w, D, t)).concat(MaybeFormatCalendarAnnotation(GetSlot(e, p), r));
}
function TemporalMonthDayToString(e) {
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "auto";
  var r = "".concat(ISODateTimePartString(GetSlot(e, s)), "-").concat(ISODateTimePartString(GetSlot(e, l)));
  var o = ToTemporalCalendarIdentifier(GetSlot(e, p));
  if ("always" === t || "critical" === t || "iso8601" !== o) {
    r = "".concat(ISOYearString(GetSlot(e, i)), "-").concat(r);
  }
  var n = FormatCalendarAnnotation(o, t);
  return n && (r += n), r;
}
function TemporalYearMonthToString(e) {
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "auto";
  var r = "".concat(ISOYearString(GetSlot(e, i)), "-").concat(ISODateTimePartString(GetSlot(e, s)));
  var o = ToTemporalCalendarIdentifier(GetSlot(e, p));
  if ("always" === t || "critical" === t || "iso8601" !== o) {
    r += "-".concat(ISODateTimePartString(GetSlot(e, l)));
  }
  var n = FormatCalendarAnnotation(o, t);
  return n && (r += n), r;
}
function TemporalZonedDateTimeToString(e, t) {
  var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "auto";
  var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : "auto";
  var a = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : "auto";
  var f = arguments.length > 5 ? arguments[5] : undefined;
  var y = GetSlot(e, S);
  if (f) {
    var _t23 = f.unit,
      _r35 = f.increment,
      _o12 = f.roundingMode,
      _a5 = RoundInstant(GetSlot(e, n), _r35, _t23, _o12);
    y = new (GetIntrinsic("%Temporal.Instant%"))(_a5);
  }
  var I = GetSlot(e, g),
    w = GetPlainDateTimeFor(I, y, "iso8601");
  var D = "".concat(ISOYearString(GetSlot(w, i)), "-").concat(ISODateTimePartString(GetSlot(w, s)), "-").concat(ISODateTimePartString(GetSlot(w, l)), "T").concat(ISODateTimePartString(GetSlot(w, d)), ":").concat(ISODateTimePartString(GetSlot(w, m))).concat(FormatSecondsStringPart(GetSlot(w, c), GetSlot(w, h), GetSlot(w, u), GetSlot(w, T), t));
  if ("never" !== a) {
    D += FormatISOTimeZoneOffsetString(GetOffsetNanosecondsFor(I, y));
  }
  if ("never" !== o) {
    D += "[".concat("critical" === o ? "!" : "").concat(ToTemporalTimeZoneIdentifier(I), "]");
  }
  return D += MaybeFormatCalendarAnnotation(GetSlot(e, p), r), D;
}
function IsTimeZoneOffsetString(e) {
  return ze.test(he(e));
}
function ParseTimeZoneOffsetString(e) {
  var t = ze.exec(he(e));
  if (!t) throw new RangeError("invalid time zone offset: ".concat(e));
  return ("-" === t[1] || "−" === t[1] ? -1 : 1) * (1e9 * (60 * (60 * +t[2] + +(t[3] || 0)) + +(t[4] || 0)) + +((t[5] || 0) + "000000000").slice(0, 9));
}
function GetCanonicalTimeZoneIdentifier(e) {
  if (IsTimeZoneOffsetString(e)) {
    return FormatTimeZoneOffsetString(ParseTimeZoneOffsetString(e));
  }
  return getIntlDateTimeFormatEnUsForTimeZone(he(e)).resolvedOptions().timeZone;
}
function GetNamedTimeZoneOffsetNanoseconds(t, r) {
  var _GetNamedTimeZoneDate = GetNamedTimeZoneDateTimeParts(t, r),
    o = _GetNamedTimeZoneDate.year,
    n = _GetNamedTimeZoneDate.month,
    a = _GetNamedTimeZoneDate.day,
    i = _GetNamedTimeZoneDate.hour,
    s = _GetNamedTimeZoneDate.minute,
    l = _GetNamedTimeZoneDate.second,
    d = _GetNamedTimeZoneDate.millisecond,
    m = _GetNamedTimeZoneDate.microsecond,
    c = _GetNamedTimeZoneDate.nanosecond,
    h = o % 400,
    u = (o - h) / 400,
    T = 146097n * Ee,
    p = GetUTCEpochNanoseconds(h, n, a, i, s, l, d, m, c),
    f = p + T * BigInt(u);
  return Number(f - r);
}
function FormatTimeZoneOffsetString(e) {
  var t = e < 0 ? "-" : "+",
    r = ae(e),
    o = r % 1e9,
    n = ie(r / 1e9) % 60,
    a = ie(r / 6e10) % 60,
    i = ISODateTimePartString(ie(r / 36e11)),
    s = ISODateTimePartString(a),
    l = ISODateTimePartString(n);
  var d = "";
  if (o) {
    var _e23 = "".concat(o).padStart(9, "0");
    for (; "0" === _e23[_e23.length - 1];) _e23 = _e23.slice(0, -1);
    d = ":".concat(l, ".").concat(_e23);
  } else n && (d = ":".concat(l));
  return "".concat(t).concat(i, ":").concat(s).concat(d);
}
function FormatISOTimeZoneOffsetString(t) {
  var r = Number(RoundNumberToIncrement(BigInt(t), be, "halfExpand"));
  var o = r < 0 ? "-" : "+";
  r = ae(r);
  var n = r / 6e10 % 60;
  return "".concat(o).concat(ISODateTimePartString(ie(r / 36e11)), ":").concat(ISODateTimePartString(n));
}
function GetUTCEpochNanoseconds(t, r, o, n, a, i, s, l, d) {
  var m = new Date();
  m.setUTCHours(n, a, i, s), m.setUTCFullYear(t, r - 1, o);
  var c = m.getTime();
  if (de(c)) return null;
  var h = BigInt(c) * Ge;
  return h = h + BigInt(l) * De, h = h + BigInt(d), h < Me || h > Re ? null : h;
}
function GetISOPartsFromEpoch(t) {
  var _divmod4 = divmod(t, Ge),
    r = _divmod4.quotient,
    o = _divmod4.remainder;
  var n = Number(r),
    a = Number(o);
  a < 0 && (a += 1e6, n -= 1);
  var i = ie(a / 1e3) % 1e3,
    s = a % 1e3,
    l = new Date(n);
  return {
    epochMilliseconds: n,
    year: l.getUTCFullYear(),
    month: l.getUTCMonth() + 1,
    day: l.getUTCDate(),
    hour: l.getUTCHours(),
    minute: l.getUTCMinutes(),
    second: l.getUTCSeconds(),
    millisecond: l.getUTCMilliseconds(),
    microsecond: i,
    nanosecond: s
  };
}
function GetNamedTimeZoneDateTimeParts(e, t) {
  var _GetISOPartsFromEpoch2 = GetISOPartsFromEpoch(t),
    r = _GetISOPartsFromEpoch2.epochMilliseconds,
    o = _GetISOPartsFromEpoch2.millisecond,
    n = _GetISOPartsFromEpoch2.microsecond,
    a = _GetISOPartsFromEpoch2.nanosecond,
    _GetFormatterParts = function GetFormatterParts(e, t) {
      var r = getIntlDateTimeFormatEnUsForTimeZone(e).format(new Date(t));
      return function parseFromEnUsFormat(e) {
        var t = e.split(/[^\w]+/);
        if (7 !== t.length) throw new RangeError("expected 7 parts in \"".concat(e));
        var r = +t[0],
          o = +t[1];
        var n = +t[2];
        var a = t[3].toUpperCase();
        if ("B" === a || "BC" === a) n = 1 - n;else if ("A" !== a && "AD" !== a) throw new RangeError("Unknown era ".concat(a, " in \"").concat(e));
        var i = +t[4];
        24 === i && (i = 0);
        var s = +t[5],
          l = +t[6];
        if (!(me(n) && me(r) && me(o) && me(i) && me(s) && me(l))) throw new RangeError("Invalid number in \"".concat(e));
        return {
          year: n,
          month: r,
          day: o,
          hour: i,
          minute: s,
          second: l
        };
      }(r);
    }(e, r),
    i = _GetFormatterParts.year,
    s = _GetFormatterParts.month,
    l = _GetFormatterParts.day,
    d = _GetFormatterParts.hour,
    m = _GetFormatterParts.minute,
    c = _GetFormatterParts.second;
  return BalanceISODateTime(i, s, l, d, m, c, o, n, a);
}
function maxJSBI(t, r) {
  return t < r ? r : t;
}
function afterLatestPossibleTzdbRuleChange() {
  return Ve() + Ze;
}
function GetNamedTimeZoneNextTransition(t, r) {
  if (r < Pe) return GetNamedTimeZoneNextTransition(t, Pe);
  var o = r + Be,
    n = maxJSBI(afterLatestPossibleTzdbRuleChange(), o);
  var a = maxJSBI(Pe, r);
  var i = GetNamedTimeZoneOffsetNanoseconds(t, a);
  var s = a,
    l = i;
  for (; i === l && BigInt(a) < n;) {
    if (s = a + Ne, s > Re) return null;
    l = GetNamedTimeZoneOffsetNanoseconds(t, s), i === l && (a = s);
  }
  if (i === l) return null;
  return bisect(function (e) {
    return GetNamedTimeZoneOffsetNanoseconds(t, e);
  }, a, s, i, l);
}
function GetNamedTimeZonePreviousTransition(t, r) {
  var o = afterLatestPossibleTzdbRuleChange(),
    a = r > o,
    i = a ? r - Be : Pe;
  if ("Africa/Casablanca" === t || "Africa/El_Aaiun" === t) {
    var _o13 = GetSlot(ToTemporalInstant("2088-01-01T00Z"), n);
    if (_o13 < r) return GetNamedTimeZonePreviousTransition(t, _o13);
  }
  var s = r - Se;
  if (s < Pe) return null;
  var l = GetNamedTimeZoneOffsetNanoseconds(t, s);
  var d = s,
    m = l;
  for (; l === m && s > i;) {
    if (d = s - Ne, d < Pe) return null;
    m = GetNamedTimeZoneOffsetNanoseconds(t, d), l === m && (s = d);
  }
  if (l === m) {
    if (a) {
      var _r36 = o - Ee;
      return GetNamedTimeZonePreviousTransition(t, _r36);
    }
    return null;
  }
  return bisect(function (e) {
    return GetNamedTimeZoneOffsetNanoseconds(t, e);
  }, d, s, m, l);
}
function LeapYear(e) {
  if (void 0 === e) return !1;
  return e % 4 == 0 && (!(e % 100 == 0) || e % 400 == 0);
}
function ISODaysInMonth(e, t) {
  return {
    standard: [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
    leapyear: [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  }[LeapYear(e) ? "leapyear" : "standard"][t - 1];
}
function DayOfWeek(e, t, r) {
  var o = t + (t < 3 ? 10 : -2),
    n = e - (t < 3 ? 1 : 0),
    a = ie(n / 100),
    i = n - 100 * a,
    s = (r + ie(2.6 * o - .2) + (i + ie(i / 4)) + (ie(a / 4) - 2 * a)) % 7;
  return s + (s <= 0 ? 7 : 0);
}
function DayOfYear(e, t, r) {
  var o = r;
  for (var _r37 = t - 1; _r37 > 0; _r37--) o += ISODaysInMonth(e, _r37);
  return o;
}
function WeekOfYear(e, t, r) {
  var o = DayOfYear(e, t, r),
    n = DayOfWeek(e, t, r) || 7,
    a = DayOfWeek(e, 1, 1),
    i = ie((o - n + 10) / 7);
  return i < 1 ? 5 === a || 6 === a && LeapYear(e - 1) ? {
    week: 53,
    year: e - 1
  } : {
    week: 52,
    year: e - 1
  } : 53 === i && (LeapYear(e) ? 366 : 365) - o < 4 - n ? {
    week: 1,
    year: e + 1
  } : {
    week: i,
    year: e
  };
}
function DurationSign(e, t, r, o, n, a, i, s, l, d) {
  for (var _i7 = 0, _arr2 = [e, t, r, o, n, a, i, s, l, d]; _i7 < _arr2.length; _i7++) {
    var _m2 = _arr2[_i7];
    if (0 !== _m2) return _m2 < 0 ? -1 : 1;
  }
  return 0;
}
function BalanceISOYearMonth(e, t) {
  var r = e,
    o = t;
  if (!me(r) || !me(o)) throw new RangeError("infinity is out of range");
  return o -= 1, r += ie(o / 12), o %= 12, o < 0 && (o += 12), o += 1, {
    year: r,
    month: o
  };
}
function BalanceISODate(e, t, r) {
  var o = e,
    n = t,
    a = r;
  if (!me(a)) throw new RangeError("infinity is out of range");
  var _BalanceISOYearMonth = BalanceISOYearMonth(o, n);
  o = _BalanceISOYearMonth.year;
  n = _BalanceISOYearMonth.month;
  var i = 146097;
  if (ae(a) > i) {
    var _e24 = le(a / i);
    o += 400 * _e24, a -= _e24 * i;
  }
  var s = 0,
    l = n > 2 ? o : o - 1;
  for (; s = LeapYear(l) ? 366 : 365, a < -s;) o -= 1, l -= 1, a += s;
  for (l += 1; s = LeapYear(l) ? 366 : 365, a > s;) o += 1, l += 1, a -= s;
  for (; a < 1;) {
    var _BalanceISOYearMonth2;
    (_BalanceISOYearMonth2 = BalanceISOYearMonth(o, n - 1), o = _BalanceISOYearMonth2.year, n = _BalanceISOYearMonth2.month), a += ISODaysInMonth(o, n);
  }
  for (; a > ISODaysInMonth(o, n);) {
    var _BalanceISOYearMonth3;
    a -= ISODaysInMonth(o, n), (_BalanceISOYearMonth3 = BalanceISOYearMonth(o, n + 1), o = _BalanceISOYearMonth3.year, n = _BalanceISOYearMonth3.month);
  }
  return {
    year: o,
    month: n,
    day: a
  };
}
function BalanceISODateTime(e, t, r, o, n, a, i, s, l) {
  var _BalanceTime = BalanceTime(o, n, a, i, s, l),
    d = _BalanceTime.deltaDays,
    m = _BalanceTime.hour,
    c = _BalanceTime.minute,
    h = _BalanceTime.second,
    u = _BalanceTime.millisecond,
    T = _BalanceTime.microsecond,
    p = _BalanceTime.nanosecond,
    _BalanceISODate = BalanceISODate(e, t, r + d),
    f = _BalanceISODate.year,
    y = _BalanceISODate.month,
    I = _BalanceISODate.day;
  return {
    year: f,
    month: y,
    day: I,
    hour: m,
    minute: c,
    second: h,
    millisecond: u,
    microsecond: T,
    nanosecond: p
  };
}
function BalanceTime(t, r, o, n, a, i) {
  var _NonNegativeBigIntDiv, _NonNegativeBigIntDiv2, _NonNegativeBigIntDiv3, _NonNegativeBigIntDiv4, _NonNegativeBigIntDiv5, _NonNegativeBigIntDiv6;
  var s,
    l = BigInt(t),
    d = BigInt(r),
    m = BigInt(o),
    c = BigInt(n),
    h = BigInt(a),
    u = BigInt(i);
  return (_NonNegativeBigIntDiv = NonNegativeBigIntDivmod(u, De), s = _NonNegativeBigIntDiv.quotient, u = _NonNegativeBigIntDiv.remainder), h = h + s, (_NonNegativeBigIntDiv2 = NonNegativeBigIntDivmod(h, De), s = _NonNegativeBigIntDiv2.quotient, h = _NonNegativeBigIntDiv2.remainder), c = c + s, (_NonNegativeBigIntDiv3 = NonNegativeBigIntDivmod(c, De), s = _NonNegativeBigIntDiv3.quotient, c = _NonNegativeBigIntDiv3.remainder), m = m + s, (_NonNegativeBigIntDiv4 = NonNegativeBigIntDivmod(m, ge), s = _NonNegativeBigIntDiv4.quotient, m = _NonNegativeBigIntDiv4.remainder), d = d + s, (_NonNegativeBigIntDiv5 = NonNegativeBigIntDivmod(d, ge), s = _NonNegativeBigIntDiv5.quotient, d = _NonNegativeBigIntDiv5.remainder), l = l + s, (_NonNegativeBigIntDiv6 = NonNegativeBigIntDivmod(l, we), s = _NonNegativeBigIntDiv6.quotient, l = _NonNegativeBigIntDiv6.remainder), {
    deltaDays: Number(s),
    hour: Number(l),
    minute: Number(d),
    second: Number(m),
    millisecond: Number(c),
    microsecond: Number(h),
    nanosecond: Number(u)
  };
}
function TotalDurationNanoseconds(t, r, o, n, a, i, s, l) {
  var d = BigInt(t);
  var m = BigInt(s);
  0 !== t && (m = BigInt(s) - BigInt(l));
  var c = BigInt(r) + d * 24n,
    h = BigInt(o) + c * ge,
    u = BigInt(n) + h * ge,
    T = BigInt(a) + u * De,
    p = BigInt(i) + T * De;
  return BigInt(m) + p * De;
}
function NanosecondsToDays(t, r) {
  var o = GetIntrinsic("%Temporal.Instant%"),
    a = se(Number(t));
  var f = BigInt(t),
    y = 864e11;
  if (0 === a) return {
    days: 0,
    nanoseconds: Ie,
    dayLengthNs: y
  };
  if (!IsTemporalZonedDateTime(r)) {
    var _divmod5;
    var _t24;
    return (_divmod5 = divmod(f, BigInt(y)), _t24 = _divmod5.quotient, f = _divmod5.remainder), {
      days: Number(_t24),
      nanoseconds: f,
      dayLengthNs: y
    };
  }
  var I = GetSlot(r, n),
    w = GetSlot(r, S),
    D = I + f,
    G = new o(D),
    v = GetSlot(r, g),
    C = GetSlot(r, p),
    O = GetPlainDateTimeFor(v, w, C),
    b = GetPlainDateTimeFor(v, G, C);
  var _DifferenceISODateTim = DifferenceISODateTime(GetSlot(O, i), GetSlot(O, s), GetSlot(O, l), GetSlot(O, d), GetSlot(O, m), GetSlot(O, c), GetSlot(O, h), GetSlot(O, u), GetSlot(O, T), GetSlot(b, i), GetSlot(b, s), GetSlot(b, l), GetSlot(b, d), GetSlot(b, m), GetSlot(b, c), GetSlot(b, h), GetSlot(b, u), GetSlot(b, T), C, "day", Te(null)),
    E = _DifferenceISODateTim.days,
    M = AddZonedDateTime(w, v, C, 0, 0, 0, E, 0, 0, 0, 0, 0, 0),
    R = BigInt(E);
  if (1 === a) for (; R > Ie && M > D;) R = R - Se, M = AddZonedDateTime(w, v, C, 0, 0, 0, Number(R), 0, 0, 0, 0, 0, 0);
  f = D - M;
  var F = !1,
    Y = new o(M);
  do {
    var _t25 = AddZonedDateTime(Y, v, C, 0, 0, 0, a, 0, 0, 0, 0, 0, 0),
      _r38 = GetSlot(Y, n);
    y = Number(_t25 - _r38), F = (f - BigInt(y)) * BigInt(a) >= Ie, F && (f = f - BigInt(y), Y = new o(_t25), R = R + BigInt(a));
  } while (F);
  if (!isZero(R) && signJSBI(R) !== a) throw new RangeError("Time zone or calendar converted nanoseconds into a number of days with the opposite sign");
  if (!isZero(f) && signJSBI(f) !== a) {
    if (isNegativeJSBI(f) && 1 === a) throw new Error("assert not reached");
    throw new RangeError("Time zone or calendar ended up with a remainder of nanoseconds with the opposite sign");
  }
  if (abs(f) >= abs(BigInt(y))) throw new Error("assert not reached");
  return {
    days: Number(R),
    nanoseconds: f,
    dayLengthNs: ae(y)
  };
}
function BalanceDuration(e, t, r, o, n, a, i, s, l) {
  var d = BalancePossiblyInfiniteDuration(e, t, r, o, n, a, i, s, l);
  if ("positive overflow" === d || "negative overflow" === d) throw new RangeError("Duration out of range");
  return d;
}
function BalancePossiblyInfiniteDuration(t, r, o, a, i, s, l, d, m) {
  var _NanosecondsToDays, _divmod6, _divmod7, _divmod8, _divmod9, _divmod10, _divmod11, _divmod12, _divmod13, _divmod14, _divmod15, _divmod16, _divmod17, _divmod18, _divmod19;
  var c,
    h,
    u,
    T,
    f,
    y,
    I = t;
  if (IsTemporalZonedDateTime(m)) {
    var _t26 = AddZonedDateTime(GetSlot(m, S), GetSlot(m, g), GetSlot(m, p), 0, 0, 0, I, r, o, a, i, s, l),
      _d = GetSlot(m, n);
    c = _t26 - _d;
  } else c = TotalDurationNanoseconds(I, r, o, a, i, s, l, 0);
  "year" === d || "month" === d || "week" === d || "day" === d ? (_NanosecondsToDays = NanosecondsToDays(c, m), I = _NanosecondsToDays.days, c = _NanosecondsToDays.nanoseconds, _NanosecondsToDays) : I = 0;
  var w = c < Ie ? -1 : 1;
  switch (c = abs(c), h = u = T = f = y = Ie, d) {
    case "year":
    case "month":
    case "week":
    case "day":
    case "hour":
      (_divmod6 = divmod(c, De), h = _divmod6.quotient, c = _divmod6.remainder), (_divmod7 = divmod(h, De), u = _divmod7.quotient, h = _divmod7.remainder), (_divmod8 = divmod(u, De), T = _divmod8.quotient, u = _divmod8.remainder), (_divmod9 = divmod(T, ge), f = _divmod9.quotient, T = _divmod9.remainder), (_divmod10 = divmod(f, ge), y = _divmod10.quotient, f = _divmod10.remainder);
      break;
    case "minute":
      (_divmod11 = divmod(c, De), h = _divmod11.quotient, c = _divmod11.remainder), (_divmod12 = divmod(h, De), u = _divmod12.quotient, h = _divmod12.remainder), (_divmod13 = divmod(u, De), T = _divmod13.quotient, u = _divmod13.remainder), (_divmod14 = divmod(T, ge), f = _divmod14.quotient, T = _divmod14.remainder);
      break;
    case "second":
      (_divmod15 = divmod(c, De), h = _divmod15.quotient, c = _divmod15.remainder), (_divmod16 = divmod(h, De), u = _divmod16.quotient, h = _divmod16.remainder), (_divmod17 = divmod(u, De), T = _divmod17.quotient, u = _divmod17.remainder);
      break;
    case "millisecond":
      (_divmod18 = divmod(c, De), h = _divmod18.quotient, c = _divmod18.remainder), (_divmod19 = divmod(h, De), u = _divmod19.quotient, h = _divmod19.remainder);
      break;
    case "microsecond":
      var _divmod20 = divmod(c, De);
      h = _divmod20.quotient;
      c = _divmod20.remainder;
      break;
    case "nanosecond":
      break;
    default:
      throw new Error("assert not reached");
  }
  var D = Number(y) * w,
    G = Number(f) * w,
    v = Number(T) * w,
    C = Number(u) * w,
    O = Number(h) * w,
    b = Number(c) * w;
  for (var _i8 = 0, _arr3 = [I, D, G, v, C, O, b]; _i8 < _arr3.length; _i8++) {
    var _e25 = _arr3[_i8];
    if (!me(_e25)) return 1 === w ? "positive overflow" : "negative overflow";
  }
  return {
    days: I,
    hours: D,
    minutes: G,
    seconds: v,
    milliseconds: C,
    microseconds: O,
    nanoseconds: b
  };
}
function UnbalanceDurationRelative(t, r, o, n, a, i) {
  var s = GetIntrinsic("%Temporal.Duration%"),
    l = DurationSign(t, r, o, n, 0, 0, 0, 0, 0, 0);
  if (0 === l) return {
    years: t,
    months: r,
    weeks: o,
    days: n
  };
  var d = BigInt(l);
  var m,
    c,
    h = BigInt(t),
    u = BigInt(r),
    T = BigInt(o),
    f = BigInt(n);
  i && (c = ToTemporalDate(i), m = GetSlot(c, p));
  var y = new s(l),
    I = new s(0, l),
    S = new s(0, 0, l);
  switch (a) {
    case "year":
      break;
    case "month":
      {
        if (!m) throw new RangeError("a starting point is required for months balancing");
        var _t27, _r39;
        for ("string" != typeof m && (_t27 = GetMethod(m, "dateAdd"), _r39 = GetMethod(m, "dateUntil")); !isZero(h);) {
          var _o14 = CalendarDateAdd(m, c, y, void 0, _t27),
            _n9 = Te(null);
          _n9.largestUnit = "month";
          var _a6 = CalendarDateUntil(m, c, _o14, _n9, _r39),
            _i9 = BigInt(GetSlot(_a6, D));
          c = _o14, u = u + _i9, h = h - d;
        }
      }
      break;
    case "week":
      {
        if (!m) throw new RangeError("a starting point is required for weeks balancing");
        var _t28 = "string" != typeof m ? GetMethod(m, "dateAdd") : void 0;
        for (; !isZero(h);) {
          var _MoveRelativeDate;
          var _r40 = void 0;
          (_MoveRelativeDate = MoveRelativeDate(m, c, y, _t28), c = _MoveRelativeDate.relativeTo, _r40 = _MoveRelativeDate.days), f = f + BigInt(_r40), h = h - d;
        }
        for (; !isZero(u);) {
          var _MoveRelativeDate2;
          var _r41 = void 0;
          (_MoveRelativeDate2 = MoveRelativeDate(m, c, I, _t28), c = _MoveRelativeDate2.relativeTo, _r41 = _MoveRelativeDate2.days), f = f + BigInt(_r41), u = u - d;
        }
        break;
      }
    default:
      {
        if (isZero(h) && isZero(u) && isZero(T)) break;
        if (!m) throw new RangeError("a starting point is required for balancing calendar units");
        var _t29 = "string" != typeof m ? GetMethod(m, "dateAdd") : void 0;
        for (; !isZero(h);) {
          var _MoveRelativeDate3;
          var _r42 = void 0;
          (_MoveRelativeDate3 = MoveRelativeDate(m, c, y, _t29), c = _MoveRelativeDate3.relativeTo, _r42 = _MoveRelativeDate3.days), f = f + BigInt(_r42), h = h - d;
        }
        for (; !isZero(u);) {
          var _MoveRelativeDate4;
          var _r43 = void 0;
          (_MoveRelativeDate4 = MoveRelativeDate(m, c, I, _t29), c = _MoveRelativeDate4.relativeTo, _r43 = _MoveRelativeDate4.days), f = f + BigInt(_r43), u = u - d;
        }
        for (; !isZero(T);) {
          var _MoveRelativeDate5;
          var _r44 = void 0;
          (_MoveRelativeDate5 = MoveRelativeDate(m, c, S, _t29), c = _MoveRelativeDate5.relativeTo, _r44 = _MoveRelativeDate5.days), f = f + BigInt(_r44), T = T - d;
        }
        break;
      }
  }
  return {
    years: Number(h),
    months: Number(u),
    weeks: Number(T),
    days: Number(f)
  };
}
function CalculateOffsetShift(e, t, r, o, n) {
  if (IsTemporalZonedDateTime(e)) {
    var _a7 = GetSlot(e, S),
      _i10 = GetSlot(e, g),
      _s = GetSlot(e, p),
      _l2 = GetOffsetNanosecondsFor(_i10, _a7),
      _d2 = AddZonedDateTime(_a7, _i10, _s, t, r, o, n, 0, 0, 0, 0, 0, 0);
    return GetOffsetNanosecondsFor(_i10, new (GetIntrinsic("%Temporal.Instant%"))(_d2)) - _l2;
  }
  return 0;
}
function CreateNegatedTemporalDuration(e) {
  return new (GetIntrinsic("%Temporal.Duration%"))(-GetSlot(e, w), -GetSlot(e, D), -GetSlot(e, G), -GetSlot(e, v), -GetSlot(e, C), -GetSlot(e, O), -GetSlot(e, b), -GetSlot(e, E), -GetSlot(e, M), -GetSlot(e, R));
}
function ConstrainToRange(e, t, r) {
  return oe(r, ne(t, e));
}
function ConstrainISODate(e, t, r) {
  var o = ConstrainToRange(t, 1, 12);
  return {
    year: e,
    month: o,
    day: ConstrainToRange(r, 1, ISODaysInMonth(e, o))
  };
}
function RejectToRange(e, t, r) {
  if (e < t || e > r) throw new RangeError("value out of range: ".concat(t, " <= ").concat(e, " <= ").concat(r));
}
function RejectISODate(e, t, r) {
  RejectToRange(t, 1, 12), RejectToRange(r, 1, ISODaysInMonth(e, t));
}
function RejectDateRange(e, t, r) {
  RejectDateTimeRange(e, t, r, 12, 0, 0, 0, 0, 0);
}
function RejectTime(e, t, r, o, n, a) {
  RejectToRange(e, 0, 23), RejectToRange(t, 0, 59), RejectToRange(r, 0, 59), RejectToRange(o, 0, 999), RejectToRange(n, 0, 999), RejectToRange(a, 0, 999);
}
function RejectDateTime(e, t, r, o, n, a, i, s, l) {
  RejectISODate(e, t, r), RejectTime(o, n, a, i, s, l);
}
function RejectDateTimeRange(e, t, r, o, n, a, i, s, l) {
  if (RejectToRange(e, Fe, Ye), e === Fe && null == GetUTCEpochNanoseconds(e, t, r + 1, o, n, a, i, s, l - 1) || e === Ye && null == GetUTCEpochNanoseconds(e, t, r - 1, o, n, a, i, s, l + 1)) throw new RangeError("DateTime outside of supported range");
}
function ValidateEpochNanoseconds(t) {
  if (t < Me || t > Re) throw new RangeError("Instant outside of supported range");
}
function RejectDuration(e, t, r, o, n, a, i, s, l, d) {
  var m = DurationSign(e, t, r, o, n, a, i, s, l, d);
  for (var _i11 = 0, _arr4 = [e, t, r, o, n, a, i, s, l, d]; _i11 < _arr4.length; _i11++) {
    var _c2 = _arr4[_i11];
    if (!me(_c2)) throw new RangeError("infinite values not allowed as duration fields");
    var _e26 = se(_c2);
    if (0 !== _e26 && _e26 !== m) throw new RangeError("mixed-sign values not allowed as duration fields");
  }
}
function DifferenceISODate(e, t, r, o, n, a, i) {
  switch (i) {
    case "year":
    case "month":
      {
        var _s2 = -CompareISODate(e, t, r, o, n, a);
        if (0 === _s2) return {
          years: 0,
          months: 0,
          weeks: 0,
          days: 0
        };
        var _l3 = {
            year: e,
            month: t,
            day: r
          },
          _d3 = {
            year: o,
            month: n,
            day: a
          };
        var _m3 = _d3.year - _l3.year,
          _c3 = AddISODate(e, t, r, _m3, 0, 0, 0, "constrain"),
          _h = -CompareISODate(_c3.year, _c3.month, _c3.day, o, n, a);
        if (0 === _h) return "year" === i ? {
          years: _m3,
          months: 0,
          weeks: 0,
          days: 0
        } : {
          years: 0,
          months: 12 * _m3,
          weeks: 0,
          days: 0
        };
        var _u = _d3.month - _l3.month;
        if (_h !== _s2 && (_m3 -= _s2, _u += 12 * _s2), _c3 = AddISODate(e, t, r, _m3, _u, 0, 0, "constrain"), _h = -CompareISODate(_c3.year, _c3.month, _c3.day, o, n, a), 0 === _h) return "year" === i ? {
          years: _m3,
          months: _u,
          weeks: 0,
          days: 0
        } : {
          years: 0,
          months: _u + 12 * _m3,
          weeks: 0,
          days: 0
        };
        _h !== _s2 && (_u -= _s2, _u === -_s2 && (_m3 -= _s2, _u = 11 * _s2), _c3 = AddISODate(e, t, r, _m3, _u, 0, 0, "constrain"));
        var _T = 0;
        return _T = _c3.month === _d3.month ? _d3.day - _c3.day : _s2 < 0 ? -_c3.day - (ISODaysInMonth(_d3.year, _d3.month) - _d3.day) : _d3.day + (ISODaysInMonth(_c3.year, _c3.month) - _c3.day), "month" === i && (_u += 12 * _m3, _m3 = 0), {
          years: _m3,
          months: _u,
          weeks: 0,
          days: _T
        };
      }
    case "week":
    case "day":
      {
        var _s3, _l4, _d4;
        CompareISODate(e, t, r, o, n, a) < 0 ? (_l4 = {
          year: e,
          month: t,
          day: r
        }, _s3 = {
          year: o,
          month: n,
          day: a
        }, _d4 = 1) : (_l4 = {
          year: o,
          month: n,
          day: a
        }, _s3 = {
          year: e,
          month: t,
          day: r
        }, _d4 = -1);
        var _m4 = DayOfYear(_s3.year, _s3.month, _s3.day) - DayOfYear(_l4.year, _l4.month, _l4.day);
        for (var _e27 = _l4.year; _e27 < _s3.year; ++_e27) _m4 += LeapYear(_e27) ? 366 : 365;
        var _c4 = 0;
        return "week" === i && (_c4 = ie(_m4 / 7), _m4 %= 7), _c4 *= _d4, _m4 *= _d4, {
          years: 0,
          months: 0,
          weeks: _c4,
          days: _m4
        };
      }
    default:
      throw new Error("assert not reached");
  }
}
function DifferenceTime(e, t, r, o, n, a, i, s, l, d, m, c) {
  var _BalanceTime2;
  var h = i - e,
    u = s - t,
    T = l - r,
    p = d - o,
    f = m - n,
    y = c - a;
  var I = DurationSign(0, 0, 0, 0, h, u, T, p, f, y);
  h *= I, u *= I, T *= I, p *= I, f *= I, y *= I;
  var S = 0;
  if ((_BalanceTime2 = BalanceTime(h, u, T, p, f, y), S = _BalanceTime2.deltaDays, h = _BalanceTime2.hour, u = _BalanceTime2.minute, T = _BalanceTime2.second, p = _BalanceTime2.millisecond, f = _BalanceTime2.microsecond, y = _BalanceTime2.nanosecond), 0 != S) throw new Error("assertion failure in DifferenceTime: _bt_.[[Days]] should be 0");
  return h *= I, u *= I, T *= I, p *= I, f *= I, y *= I, {
    hours: h,
    minutes: u,
    seconds: T,
    milliseconds: p,
    microseconds: f,
    nanoseconds: y
  };
}
function DifferenceInstant(t, r, o, n, a, i) {
  var _RoundDuration2;
  var s = r - t;
  var l = 0,
    d = 0,
    m = Number(s % De),
    c = Number(s / De % De),
    h = Number(s / Ge % De),
    u = Number(s / ve);
  return (_RoundDuration2 = RoundDuration(0, 0, 0, 0, 0, 0, u, h, c, m, o, n, i), l = _RoundDuration2.hours, d = _RoundDuration2.minutes, u = _RoundDuration2.seconds, h = _RoundDuration2.milliseconds, c = _RoundDuration2.microseconds, m = _RoundDuration2.nanoseconds), BalanceDuration(0, l, d, u, h, c, m, a);
}
function DifferenceISODateTime(e, t, r, o, n, a, i, s, l, d, m, c, h, u, T, p, f, y, I, S, g) {
  var _BalanceISODate2, _BalanceDuration, _BalanceDuration2;
  var w = e,
    D = t,
    G = r,
    _DifferenceTime = DifferenceTime(o, n, a, i, s, l, h, u, T, p, f, y),
    v = _DifferenceTime.hours,
    C = _DifferenceTime.minutes,
    O = _DifferenceTime.seconds,
    b = _DifferenceTime.milliseconds,
    E = _DifferenceTime.microseconds,
    M = _DifferenceTime.nanoseconds;
  var R = DurationSign(0, 0, 0, 0, v, C, O, b, E, M);
  CompareISODate(d, m, c, w, D, G) === -R && ((_BalanceISODate2 = BalanceISODate(w, D, G - R), w = _BalanceISODate2.year, D = _BalanceISODate2.month, G = _BalanceISODate2.day), (_BalanceDuration = BalanceDuration(-R, v, C, O, b, E, M, S), v = _BalanceDuration.hours, C = _BalanceDuration.minutes, O = _BalanceDuration.seconds, b = _BalanceDuration.milliseconds, E = _BalanceDuration.microseconds, M = _BalanceDuration.nanoseconds));
  var F = CreateTemporalDate(w, D, G, I),
    Y = CreateTemporalDate(d, m, c, I),
    P = LargerOfTwoTemporalUnits("day", S),
    Z = CopyOptions(g);
  Z.largestUnit = P;
  var _CalendarDateUntil = CalendarDateUntil(I, F, Y, Z),
    B = _CalendarDateUntil.years,
    N = _CalendarDateUntil.months,
    j = _CalendarDateUntil.weeks,
    $ = _CalendarDateUntil.days;
  return (_BalanceDuration2 = BalanceDuration($, v, C, O, b, E, M, S), $ = _BalanceDuration2.days, v = _BalanceDuration2.hours, C = _BalanceDuration2.minutes, O = _BalanceDuration2.seconds, b = _BalanceDuration2.milliseconds, E = _BalanceDuration2.microseconds, M = _BalanceDuration2.nanoseconds), {
    years: B,
    months: N,
    weeks: j,
    days: $,
    hours: v,
    minutes: C,
    seconds: O,
    milliseconds: b,
    microseconds: E,
    nanoseconds: M
  };
}
function DifferenceZonedDateTime(t, r, o, n, a, p) {
  var f = r - t;
  if (f === Ie) return {
    years: 0,
    months: 0,
    weeks: 0,
    days: 0,
    hours: 0,
    minutes: 0,
    seconds: 0,
    milliseconds: 0,
    microseconds: 0,
    nanoseconds: 0
  };
  var y = GetIntrinsic("%Temporal.Instant%"),
    I = new y(t),
    S = new y(r),
    g = GetPlainDateTimeFor(o, I, n),
    w = GetPlainDateTimeFor(o, S, n);
  var _DifferenceISODateTim2 = DifferenceISODateTime(GetSlot(g, i), GetSlot(g, s), GetSlot(g, l), GetSlot(g, d), GetSlot(g, m), GetSlot(g, c), GetSlot(g, h), GetSlot(g, u), GetSlot(g, T), GetSlot(w, i), GetSlot(w, s), GetSlot(w, l), GetSlot(w, d), GetSlot(w, m), GetSlot(w, c), GetSlot(w, h), GetSlot(w, u), GetSlot(w, T), n, a, p),
    D = _DifferenceISODateTim2.years,
    G = _DifferenceISODateTim2.months,
    v = _DifferenceISODateTim2.weeks,
    C = _DifferenceISODateTim2.days;
  var O = AddZonedDateTime(I, o, n, D, G, v, 0, 0, 0, 0, 0, 0, 0);
  var b = r - O;
  var E = CreateTemporalZonedDateTime(O, o, n);
  var _NanosecondsToDays2 = NanosecondsToDays(b, E);
  b = _NanosecondsToDays2.nanoseconds;
  C = _NanosecondsToDays2.days;
  var _BalanceDuration3 = BalanceDuration(0, 0, 0, 0, 0, 0, Number(b), "hour"),
    M = _BalanceDuration3.hours,
    R = _BalanceDuration3.minutes,
    F = _BalanceDuration3.seconds,
    Y = _BalanceDuration3.milliseconds,
    P = _BalanceDuration3.microseconds,
    Z = _BalanceDuration3.nanoseconds;
  return {
    years: D,
    months: G,
    weeks: v,
    days: C,
    hours: M,
    minutes: R,
    seconds: F,
    milliseconds: Y,
    microseconds: P,
    nanoseconds: Z
  };
}
function GetDifferenceSettings(e, t, r, o, n, a) {
  var i = Ue.reduce(function (e, t) {
    var n = t[0],
      a = t[1],
      i = t[2];
    return "datetime" !== r && i !== r || o.includes(a) || e.push(a, n), e;
  }, []);
  var s = GetTemporalUnit(t, "largestUnit", r, "auto");
  if (o.includes(s)) throw new RangeError("largestUnit must be one of ".concat(i.join(", "), ", not ").concat(s));
  var l = ToTemporalRoundingIncrement(t);
  var d = ToTemporalRoundingMode(t, "trunc");
  "since" === e && (d = function NegateTemporalRoundingMode(e) {
    switch (e) {
      case "ceil":
        return "floor";
      case "floor":
        return "ceil";
      case "halfCeil":
        return "halfFloor";
      case "halfFloor":
        return "halfCeil";
      default:
        return e;
    }
  }(d));
  var m = GetTemporalUnit(t, "smallestUnit", r, n);
  if (o.includes(m)) throw new RangeError("smallestUnit must be one of ".concat(i.join(", "), ", not ").concat(m));
  var c = LargerOfTwoTemporalUnits(a, m);
  if ("auto" === s && (s = c), LargerOfTwoTemporalUnits(s, m) !== s) throw new RangeError("largestUnit ".concat(s, " cannot be smaller than smallestUnit ").concat(m));
  var h = {
    hour: 24,
    minute: 60,
    second: 60,
    millisecond: 1e3,
    microsecond: 1e3,
    nanosecond: 1e3
  }[m];
  return void 0 !== h && ValidateTemporalRoundingIncrement(l, h, !1), {
    largestUnit: s,
    roundingIncrement: l,
    roundingMode: d,
    smallestUnit: m
  };
}
function DifferenceTemporalInstant(e, t, r, o) {
  var a = "since" === e ? -1 : 1,
    i = ToTemporalInstant(r),
    s = GetDifferenceSettings(e, CopyOptions(o), "time", [], "nanosecond", "second"),
    l = GetSlot(t, n),
    d = GetSlot(i, n);
  var _DifferenceInstant = DifferenceInstant(l, d, s.roundingIncrement, s.smallestUnit, s.largestUnit, s.roundingMode),
    m = _DifferenceInstant.hours,
    c = _DifferenceInstant.minutes,
    h = _DifferenceInstant.seconds,
    u = _DifferenceInstant.milliseconds,
    T = _DifferenceInstant.microseconds,
    p = _DifferenceInstant.nanoseconds;
  return new (GetIntrinsic("%Temporal.Duration%"))(0, 0, 0, 0, a * m, a * c, a * h, a * u, a * T, a * p);
}
function DifferenceTemporalPlainDate(e, t, r, o) {
  var _RoundDuration3;
  var n = "since" === e ? -1 : 1,
    a = ToTemporalDate(r),
    i = GetSlot(t, p);
  ThrowIfCalendarsNotEqual(i, GetSlot(a, p), "compute difference between dates");
  var s = CopyOptions(o),
    l = GetDifferenceSettings(e, s, "date", [], "day", "day");
  s.largestUnit = l.largestUnit;
  var _CalendarDateUntil2 = CalendarDateUntil(i, t, a, s),
    d = _CalendarDateUntil2.years,
    m = _CalendarDateUntil2.months,
    c = _CalendarDateUntil2.weeks,
    h = _CalendarDateUntil2.days;
  "day" === l.smallestUnit && 1 === l.roundingIncrement || (_RoundDuration3 = RoundDuration(d, m, c, h, 0, 0, 0, 0, 0, 0, l.roundingIncrement, l.smallestUnit, l.roundingMode, t), d = _RoundDuration3.years, m = _RoundDuration3.months, c = _RoundDuration3.weeks, h = _RoundDuration3.days, _RoundDuration3);
  return new (GetIntrinsic("%Temporal.Duration%"))(n * d, n * m, n * c, n * h, 0, 0, 0, 0, 0, 0);
}
function DifferenceTemporalPlainDateTime(e, t, r, o) {
  var _RoundDuration4, _BalanceDuration4;
  var n = "since" === e ? -1 : 1,
    a = ToTemporalDateTime(r),
    f = GetSlot(t, p);
  ThrowIfCalendarsNotEqual(f, GetSlot(a, p), "compute difference between dates");
  var y = CopyOptions(o),
    I = GetDifferenceSettings(e, y, "datetime", [], "nanosecond", "day");
  var _DifferenceISODateTim3 = DifferenceISODateTime(GetSlot(t, i), GetSlot(t, s), GetSlot(t, l), GetSlot(t, d), GetSlot(t, m), GetSlot(t, c), GetSlot(t, h), GetSlot(t, u), GetSlot(t, T), GetSlot(a, i), GetSlot(a, s), GetSlot(a, l), GetSlot(a, d), GetSlot(a, m), GetSlot(a, c), GetSlot(a, h), GetSlot(a, u), GetSlot(a, T), f, I.largestUnit, y),
    S = _DifferenceISODateTim3.years,
    g = _DifferenceISODateTim3.months,
    w = _DifferenceISODateTim3.weeks,
    D = _DifferenceISODateTim3.days,
    G = _DifferenceISODateTim3.hours,
    v = _DifferenceISODateTim3.minutes,
    C = _DifferenceISODateTim3.seconds,
    O = _DifferenceISODateTim3.milliseconds,
    b = _DifferenceISODateTim3.microseconds,
    E = _DifferenceISODateTim3.nanoseconds;
  var M = TemporalDateTimeToDate(t);
  (_RoundDuration4 = RoundDuration(S, g, w, D, G, v, C, O, b, E, I.roundingIncrement, I.smallestUnit, I.roundingMode, M), S = _RoundDuration4.years, g = _RoundDuration4.months, w = _RoundDuration4.weeks, D = _RoundDuration4.days, G = _RoundDuration4.hours, v = _RoundDuration4.minutes, C = _RoundDuration4.seconds, O = _RoundDuration4.milliseconds, b = _RoundDuration4.microseconds, E = _RoundDuration4.nanoseconds), (_BalanceDuration4 = BalanceDuration(D, G, v, C, O, b, E, I.largestUnit), D = _BalanceDuration4.days, G = _BalanceDuration4.hours, v = _BalanceDuration4.minutes, C = _BalanceDuration4.seconds, O = _BalanceDuration4.milliseconds, b = _BalanceDuration4.microseconds, E = _BalanceDuration4.nanoseconds);
  return new (GetIntrinsic("%Temporal.Duration%"))(n * S, n * g, n * w, n * D, n * G, n * v, n * C, n * O, n * b, n * E);
}
function DifferenceTemporalPlainTime(e, t, r, o) {
  var _RoundDuration5, _BalanceDuration5;
  var n = "since" === e ? -1 : 1,
    a = ToTemporalTime(r),
    i = GetDifferenceSettings(e, CopyOptions(o), "time", [], "nanosecond", "hour");
  var _DifferenceTime2 = DifferenceTime(GetSlot(t, d), GetSlot(t, m), GetSlot(t, c), GetSlot(t, h), GetSlot(t, u), GetSlot(t, T), GetSlot(a, d), GetSlot(a, m), GetSlot(a, c), GetSlot(a, h), GetSlot(a, u), GetSlot(a, T)),
    s = _DifferenceTime2.hours,
    l = _DifferenceTime2.minutes,
    p = _DifferenceTime2.seconds,
    f = _DifferenceTime2.milliseconds,
    y = _DifferenceTime2.microseconds,
    I = _DifferenceTime2.nanoseconds;
  (_RoundDuration5 = RoundDuration(0, 0, 0, 0, s, l, p, f, y, I, i.roundingIncrement, i.smallestUnit, i.roundingMode), s = _RoundDuration5.hours, l = _RoundDuration5.minutes, p = _RoundDuration5.seconds, f = _RoundDuration5.milliseconds, y = _RoundDuration5.microseconds, I = _RoundDuration5.nanoseconds), (_BalanceDuration5 = BalanceDuration(0, s, l, p, f, y, I, i.largestUnit), s = _BalanceDuration5.hours, l = _BalanceDuration5.minutes, p = _BalanceDuration5.seconds, f = _BalanceDuration5.milliseconds, y = _BalanceDuration5.microseconds, I = _BalanceDuration5.nanoseconds);
  return new (GetIntrinsic("%Temporal.Duration%"))(0, 0, 0, 0, n * s, n * l, n * p, n * f, n * y, n * I);
}
function DifferenceTemporalPlainYearMonth(e, t, r, o) {
  var _RoundDuration6;
  var n = "since" === e ? -1 : 1,
    a = ToTemporalYearMonth(r),
    i = GetSlot(t, p);
  ThrowIfCalendarsNotEqual(i, GetSlot(a, p), "compute difference between months");
  var s = CopyOptions(o),
    l = GetDifferenceSettings(e, s, "date", ["week", "day"], "month", "year");
  s.largestUnit = l.largestUnit;
  var d = CalendarFields(i, ["monthCode", "year"]),
    m = PrepareTemporalFields(t, d, []);
  m.day = 1;
  var c = CalendarDateFromFields(i, m),
    h = PrepareTemporalFields(a, d, []);
  h.day = 1;
  var u = CalendarDateFromFields(i, h);
  var _CalendarDateUntil3 = CalendarDateUntil(i, c, u, s),
    T = _CalendarDateUntil3.years,
    f = _CalendarDateUntil3.months;
  "month" === l.smallestUnit && 1 === l.roundingIncrement || (_RoundDuration6 = RoundDuration(T, f, 0, 0, 0, 0, 0, 0, 0, 0, l.roundingIncrement, l.smallestUnit, l.roundingMode, c), T = _RoundDuration6.years, f = _RoundDuration6.months, _RoundDuration6);
  return new (GetIntrinsic("%Temporal.Duration%"))(n * T, n * f, 0, 0, 0, 0, 0, 0, 0, 0);
}
function DifferenceTemporalZonedDateTime(e, t, r, o) {
  var _DifferenceInstant2;
  var a = "since" === e ? -1 : 1,
    i = ToTemporalZonedDateTime(r),
    s = GetSlot(t, p);
  ThrowIfCalendarsNotEqual(s, GetSlot(i, p), "compute difference between dates");
  var l = CopyOptions(o),
    d = GetDifferenceSettings(e, l, "datetime", [], "nanosecond", "hour");
  l.largestUnit = d.largestUnit;
  var m = GetSlot(t, n),
    c = GetSlot(i, n);
  var h, u, T, f, y, I, S, w, D, G;
  if ("year" !== d.largestUnit && "month" !== d.largestUnit && "week" !== d.largestUnit && "day" !== d.largestUnit) h = 0, u = 0, T = 0, f = 0, (_DifferenceInstant2 = DifferenceInstant(m, c, d.roundingIncrement, d.smallestUnit, d.largestUnit, d.roundingMode), y = _DifferenceInstant2.hours, I = _DifferenceInstant2.minutes, S = _DifferenceInstant2.seconds, w = _DifferenceInstant2.milliseconds, D = _DifferenceInstant2.microseconds, G = _DifferenceInstant2.nanoseconds);else {
    var _DifferenceZonedDateT, _RoundDuration7, _AdjustRoundedDuratio;
    var _e28 = GetSlot(t, g);
    if (!TimeZoneEquals(_e28, GetSlot(i, g))) throw new RangeError("When calculating difference between time zones, largestUnit must be 'hours' or smaller because day lengths can vary between time zones due to DST or time zone offset changes.");
    (_DifferenceZonedDateT = DifferenceZonedDateTime(m, c, _e28, s, d.largestUnit, l), h = _DifferenceZonedDateT.years, u = _DifferenceZonedDateT.months, T = _DifferenceZonedDateT.weeks, f = _DifferenceZonedDateT.days, y = _DifferenceZonedDateT.hours, I = _DifferenceZonedDateT.minutes, S = _DifferenceZonedDateT.seconds, w = _DifferenceZonedDateT.milliseconds, D = _DifferenceZonedDateT.microseconds, G = _DifferenceZonedDateT.nanoseconds), (_RoundDuration7 = RoundDuration(h, u, T, f, y, I, S, w, D, G, d.roundingIncrement, d.smallestUnit, d.roundingMode, t), h = _RoundDuration7.years, u = _RoundDuration7.months, T = _RoundDuration7.weeks, f = _RoundDuration7.days, y = _RoundDuration7.hours, I = _RoundDuration7.minutes, S = _RoundDuration7.seconds, w = _RoundDuration7.milliseconds, D = _RoundDuration7.microseconds, G = _RoundDuration7.nanoseconds), (_AdjustRoundedDuratio = AdjustRoundedDurationDays(h, u, T, f, y, I, S, w, D, G, d.roundingIncrement, d.smallestUnit, d.roundingMode, t), h = _AdjustRoundedDuratio.years, u = _AdjustRoundedDuratio.months, T = _AdjustRoundedDuratio.weeks, f = _AdjustRoundedDuratio.days, y = _AdjustRoundedDuratio.hours, I = _AdjustRoundedDuratio.minutes, S = _AdjustRoundedDuratio.seconds, w = _AdjustRoundedDuratio.milliseconds, D = _AdjustRoundedDuratio.microseconds, G = _AdjustRoundedDuratio.nanoseconds);
  }
  return new (GetIntrinsic("%Temporal.Duration%"))(a * h, a * u, a * T, a * f, a * y, a * I, a * S, a * w, a * D, a * G);
}
function AddISODate(e, t, r, o, n, a, i, s) {
  var _BalanceISOYearMonth4, _RegulateISODate, _BalanceISODate3;
  var l = e,
    d = t,
    m = r,
    c = a,
    h = i;
  return l += o, d += n, (_BalanceISOYearMonth4 = BalanceISOYearMonth(l, d), l = _BalanceISOYearMonth4.year, d = _BalanceISOYearMonth4.month), (_RegulateISODate = RegulateISODate(l, d, m, s), l = _RegulateISODate.year, d = _RegulateISODate.month, m = _RegulateISODate.day), h += 7 * c, m += h, (_BalanceISODate3 = BalanceISODate(l, d, m), l = _BalanceISODate3.year, d = _BalanceISODate3.month, m = _BalanceISODate3.day), {
    year: l,
    month: d,
    day: m
  };
}
function AddTime(e, t, r, o, n, a, i, s, l, d, m, c) {
  var _BalanceTime3;
  var h = e,
    u = t,
    T = r,
    p = o,
    f = n,
    y = a;
  h += i, u += s, T += l, p += d, f += m, y += c;
  var I = 0;
  return (_BalanceTime3 = BalanceTime(h, u, T, p, f, y), I = _BalanceTime3.deltaDays, h = _BalanceTime3.hour, u = _BalanceTime3.minute, T = _BalanceTime3.second, p = _BalanceTime3.millisecond, f = _BalanceTime3.microsecond, y = _BalanceTime3.nanosecond), {
    deltaDays: I,
    hour: h,
    minute: u,
    second: T,
    millisecond: p,
    microsecond: f,
    nanosecond: y
  };
}
function AddDuration(t, r, o, a, i, s, l, d, m, c, h, u, T, f, y, I, w, D, G, v, C) {
  var O = LargerOfTwoTemporalUnits(DefaultTemporalLargestUnit(t, r, o, a, i, s, l, d, m, c), DefaultTemporalLargestUnit(h, u, T, f, y, I, w, D, G, v));
  var b, E, M, R, F, Y, P, Z, B, N;
  if (C) {
    if (IsTemporalDate(C)) {
      var _CalendarDateUntil4, _BalanceDuration6;
      var _n10 = GetIntrinsic("%Temporal.Duration%"),
        _S2 = GetSlot(C, p),
        _g = new _n10(t, r, o, a, 0, 0, 0, 0, 0, 0),
        _j = new _n10(h, u, T, f, 0, 0, 0, 0, 0, 0),
        _$ = "string" != typeof _S2 ? GetMethod(_S2, "dateAdd") : void 0,
        _k = CalendarDateAdd(_S2, C, _g, void 0, _$),
        _U = CalendarDateAdd(_S2, _k, _j, void 0, _$),
        _A = LargerOfTwoTemporalUnits("day", O),
        _L = Te(null);
      _L.largestUnit = _A, (_CalendarDateUntil4 = CalendarDateUntil(_S2, C, _U, _L), b = _CalendarDateUntil4.years, E = _CalendarDateUntil4.months, M = _CalendarDateUntil4.weeks, R = _CalendarDateUntil4.days), (_BalanceDuration6 = BalanceDuration(R, BigInt(i) + BigInt(y), BigInt(s) + BigInt(I), BigInt(l) + BigInt(w), BigInt(d) + BigInt(D), BigInt(m) + BigInt(G), BigInt(c) + BigInt(v), O), R = _BalanceDuration6.days, F = _BalanceDuration6.hours, Y = _BalanceDuration6.minutes, P = _BalanceDuration6.seconds, Z = _BalanceDuration6.milliseconds, B = _BalanceDuration6.microseconds, N = _BalanceDuration6.nanoseconds);
    } else {
      var _DifferenceInstant3, _DifferenceZonedDateT2;
      var _e29 = GetIntrinsic("%Temporal.Instant%"),
        _j2 = GetSlot(C, g),
        _$2 = GetSlot(C, p),
        _k2 = AddZonedDateTime(GetSlot(C, S), _j2, _$2, t, r, o, a, i, s, l, d, m, c),
        _U2 = AddZonedDateTime(new _e29(_k2), _j2, _$2, h, u, T, f, y, I, w, D, G, v);
      "year" !== O && "month" !== O && "week" !== O && "day" !== O ? (b = 0, E = 0, M = 0, R = 0, (_DifferenceInstant3 = DifferenceInstant(GetSlot(C, n), _U2, 1, "nanosecond", O, "halfExpand"), F = _DifferenceInstant3.hours, Y = _DifferenceInstant3.minutes, P = _DifferenceInstant3.seconds, Z = _DifferenceInstant3.milliseconds, B = _DifferenceInstant3.microseconds, N = _DifferenceInstant3.nanoseconds)) : (_DifferenceZonedDateT2 = DifferenceZonedDateTime(GetSlot(C, n), _U2, _j2, _$2, O, Te(null)), b = _DifferenceZonedDateT2.years, E = _DifferenceZonedDateT2.months, M = _DifferenceZonedDateT2.weeks, R = _DifferenceZonedDateT2.days, F = _DifferenceZonedDateT2.hours, Y = _DifferenceZonedDateT2.minutes, P = _DifferenceZonedDateT2.seconds, Z = _DifferenceZonedDateT2.milliseconds, B = _DifferenceZonedDateT2.microseconds, N = _DifferenceZonedDateT2.nanoseconds, _DifferenceZonedDateT2);
    }
  } else {
    var _BalanceDuration7;
    if ("year" === O || "month" === O || "week" === O) throw new RangeError("relativeTo is required for years, months, or weeks arithmetic");
    b = E = M = 0, (_BalanceDuration7 = BalanceDuration(a + f, BigInt(i) + BigInt(y), BigInt(s) + BigInt(I), BigInt(l) + BigInt(w), BigInt(d) + BigInt(D), BigInt(m) + BigInt(G), BigInt(c) + BigInt(v), O), R = _BalanceDuration7.days, F = _BalanceDuration7.hours, Y = _BalanceDuration7.minutes, P = _BalanceDuration7.seconds, Z = _BalanceDuration7.milliseconds, B = _BalanceDuration7.microseconds, N = _BalanceDuration7.nanoseconds);
  }
  return RejectDuration(b, E, M, R, F, Y, P, Z, B, N), {
    years: b,
    months: E,
    weeks: M,
    days: R,
    hours: F,
    minutes: Y,
    seconds: P,
    milliseconds: Z,
    microseconds: B,
    nanoseconds: N
  };
}
function AddInstant(t, r, o, n, a, i, s) {
  var l = Ie;
  l = l + BigInt(s), l = l + BigInt(i) * De, l = l + BigInt(a) * Ge, l = l + BigInt(n) * ve, l = l + BigInt(o) * 60000000000n, l = l + BigInt(r) * 3600000000000n;
  var d = t + l;
  return ValidateEpochNanoseconds(d), d;
}
function AddDateTime(e, t, r, o, n, a, d, m, c, h, u, T, p, f, y, I, S, g, w, D, G) {
  var v = f,
    _AddTime = AddTime(o, n, a, d, m, c, y, I, S, g, w, D),
    C = _AddTime.deltaDays,
    O = _AddTime.hour,
    b = _AddTime.minute,
    E = _AddTime.second,
    M = _AddTime.millisecond,
    R = _AddTime.microsecond,
    F = _AddTime.nanosecond;
  v += C;
  var Y = GetIntrinsic("%Temporal.Duration%"),
    P = CalendarDateAdd(h, CreateTemporalDate(e, t, r, h), new Y(u, T, p, v, 0, 0, 0, 0, 0, 0), G);
  return {
    year: GetSlot(P, i),
    month: GetSlot(P, s),
    day: GetSlot(P, l),
    hour: O,
    minute: b,
    second: E,
    millisecond: M,
    microsecond: R,
    nanosecond: F
  };
}
function AddZonedDateTime(e, t, r, o, a, p, f, y, I, S, g, w, D, G) {
  var v = GetIntrinsic("%Temporal.Duration%");
  if (0 === DurationSign(o, a, p, f, 0, 0, 0, 0, 0, 0)) return AddInstant(GetSlot(e, n), y, I, S, g, w, D);
  var C = GetPlainDateTimeFor(t, e, r),
    O = CalendarDateAdd(r, CreateTemporalDate(GetSlot(C, i), GetSlot(C, s), GetSlot(C, l), r), new v(o, a, p, f, 0, 0, 0, 0, 0, 0), G),
    b = CreateTemporalDateTime(GetSlot(O, i), GetSlot(O, s), GetSlot(O, l), GetSlot(C, d), GetSlot(C, m), GetSlot(C, c), GetSlot(C, h), GetSlot(C, u), GetSlot(C, T), r);
  return AddInstant(GetSlot(GetInstantFor(t, b, "compatible"), n), y, I, S, g, w, D);
}
function AddDurationToOrSubtractDurationFromDuration(e, t, r, o) {
  var n = "subtract" === e ? -1 : 1;
  var _ToTemporalDurationRe2 = ToTemporalDurationRecord(r),
    a = _ToTemporalDurationRe2.years,
    i = _ToTemporalDurationRe2.months,
    s = _ToTemporalDurationRe2.weeks,
    l = _ToTemporalDurationRe2.days,
    d = _ToTemporalDurationRe2.hours,
    m = _ToTemporalDurationRe2.minutes,
    c = _ToTemporalDurationRe2.seconds,
    h = _ToTemporalDurationRe2.milliseconds,
    u = _ToTemporalDurationRe2.microseconds,
    T = _ToTemporalDurationRe2.nanoseconds;
  var p = ToRelativeTemporalObject(GetOptionsObject(o));
  var _AddDuration = AddDuration(GetSlot(t, w), GetSlot(t, D), GetSlot(t, G), GetSlot(t, v), GetSlot(t, C), GetSlot(t, O), GetSlot(t, b), GetSlot(t, E), GetSlot(t, M), GetSlot(t, R), n * a, n * i, n * s, n * l, n * d, n * m, n * c, n * h, n * u, n * T, p);
  a = _AddDuration.years;
  i = _AddDuration.months;
  s = _AddDuration.weeks;
  l = _AddDuration.days;
  d = _AddDuration.hours;
  m = _AddDuration.minutes;
  c = _AddDuration.seconds;
  h = _AddDuration.milliseconds;
  u = _AddDuration.microseconds;
  T = _AddDuration.nanoseconds;
  return new (GetIntrinsic("%Temporal.Duration%"))(a, i, s, l, d, m, c, h, u, T);
}
function AddDurationToOrSubtractDurationFromInstant(e, t, r) {
  var o = "subtract" === e ? -1 : 1,
    _ToLimitedTemporalDur = function ToLimitedTemporalDuration(e, t) {
      var r = ToTemporalDurationRecord(e);
      var _iterator14 = _createForOfIteratorHelper(t),
        _step14;
      try {
        for (_iterator14.s(); !(_step14 = _iterator14.n()).done;) {
          var _e30 = _step14.value;
          if (0 !== r[_e30]) throw new RangeError("Duration field ".concat(_e30, " not supported by Temporal.Instant. Try Temporal.ZonedDateTime instead."));
        }
      } catch (err) {
        _iterator14.e(err);
      } finally {
        _iterator14.f();
      }
      return r;
    }(r, ["years", "months", "weeks", "days"]),
    a = _ToLimitedTemporalDur.hours,
    i = _ToLimitedTemporalDur.minutes,
    s = _ToLimitedTemporalDur.seconds,
    l = _ToLimitedTemporalDur.milliseconds,
    d = _ToLimitedTemporalDur.microseconds,
    m = _ToLimitedTemporalDur.nanoseconds,
    c = AddInstant(GetSlot(t, n), o * a, o * i, o * s, o * l, o * d, o * m);
  return new (GetIntrinsic("%Temporal.Instant%"))(c);
}
function AddDurationToOrSubtractDurationFromPlainDateTime(e, t, r, o) {
  var n = "subtract" === e ? -1 : 1,
    _ToTemporalDurationRe3 = ToTemporalDurationRecord(r),
    a = _ToTemporalDurationRe3.years,
    f = _ToTemporalDurationRe3.months,
    y = _ToTemporalDurationRe3.weeks,
    I = _ToTemporalDurationRe3.days,
    S = _ToTemporalDurationRe3.hours,
    g = _ToTemporalDurationRe3.minutes,
    w = _ToTemporalDurationRe3.seconds,
    D = _ToTemporalDurationRe3.milliseconds,
    G = _ToTemporalDurationRe3.microseconds,
    v = _ToTemporalDurationRe3.nanoseconds,
    C = GetOptionsObject(o),
    O = GetSlot(t, p),
    _AddDateTime = AddDateTime(GetSlot(t, i), GetSlot(t, s), GetSlot(t, l), GetSlot(t, d), GetSlot(t, m), GetSlot(t, c), GetSlot(t, h), GetSlot(t, u), GetSlot(t, T), O, n * a, n * f, n * y, n * I, n * S, n * g, n * w, n * D, n * G, n * v, C),
    b = _AddDateTime.year,
    E = _AddDateTime.month,
    M = _AddDateTime.day,
    R = _AddDateTime.hour,
    F = _AddDateTime.minute,
    Y = _AddDateTime.second,
    P = _AddDateTime.millisecond,
    Z = _AddDateTime.microsecond,
    B = _AddDateTime.nanosecond;
  return CreateTemporalDateTime(b, E, M, R, F, Y, P, Z, B, O);
}
function AddDurationToOrSubtractDurationFromPlainTime(e, t, r) {
  var o = "subtract" === e ? -1 : 1,
    _ToTemporalDurationRe4 = ToTemporalDurationRecord(r),
    n = _ToTemporalDurationRe4.hours,
    a = _ToTemporalDurationRe4.minutes,
    i = _ToTemporalDurationRe4.seconds,
    s = _ToTemporalDurationRe4.milliseconds,
    l = _ToTemporalDurationRe4.microseconds,
    p = _ToTemporalDurationRe4.nanoseconds;
  var _AddTime2 = AddTime(GetSlot(t, d), GetSlot(t, m), GetSlot(t, c), GetSlot(t, h), GetSlot(t, u), GetSlot(t, T), o * n, o * a, o * i, o * s, o * l, o * p),
    f = _AddTime2.hour,
    y = _AddTime2.minute,
    I = _AddTime2.second,
    S = _AddTime2.millisecond,
    g = _AddTime2.microsecond,
    w = _AddTime2.nanosecond;
  var _RegulateTime3 = RegulateTime(f, y, I, S, g, w, "reject");
  f = _RegulateTime3.hour;
  y = _RegulateTime3.minute;
  I = _RegulateTime3.second;
  S = _RegulateTime3.millisecond;
  g = _RegulateTime3.microsecond;
  w = _RegulateTime3.nanosecond;
  return new (GetIntrinsic("%Temporal.PlainTime%"))(f, y, I, S, g, w);
}
function AddDurationToOrSubtractDurationFromPlainYearMonth(e, t, r, o) {
  var n = ToTemporalDurationRecord(r);
  "subtract" === e && (n = {
    years: -n.years,
    months: -n.months,
    weeks: -n.weeks,
    days: -n.days,
    hours: -n.hours,
    minutes: -n.minutes,
    seconds: -n.seconds,
    milliseconds: -n.milliseconds,
    microseconds: -n.microseconds,
    nanoseconds: -n.nanoseconds
  });
  var _n11 = n,
    a = _n11.years,
    i = _n11.months,
    s = _n11.weeks,
    l = _n11.days,
    d = _n11.hours,
    m = _n11.minutes,
    c = _n11.seconds,
    h = _n11.milliseconds,
    u = _n11.microseconds,
    T = _n11.nanoseconds;
  var _BalanceDuration8 = BalanceDuration(l, d, m, c, h, u, T, "day");
  l = _BalanceDuration8.days;
  var f = GetOptionsObject(o),
    y = GetSlot(t, p),
    I = CalendarFields(y, ["monthCode", "year"]),
    S = PrepareTemporalFields(t, I, []),
    g = Te(null);
  CopyDataProperties(g, S, []), S.day = 1;
  var w = CalendarDateFromFields(y, S);
  var D = DurationSign(a, i, s, l, 0, 0, 0, 0, 0, 0),
    G = GetMethod(y, "dateAdd"),
    v = GetIntrinsic("%Temporal.Duration%");
  if (D < 0) {
    var _e31 = CalendarDateAdd(y, w, new v(0, 1, 0, 0, 0, 0, 0, 0, 0, 0), void 0, G),
      _t30 = CalendarDateAdd(y, _e31, new v(0, 0, 0, -1, 0, 0, 0, 0, 0, 0), void 0, G);
    g.day = CalendarDay(y, _t30), w = CalendarDateFromFields(y, g);
  }
  var C = new v(a, i, s, l, 0, 0, 0, 0, 0, 0),
    O = CopyOptions(f);
  return CalendarYearMonthFromFields(y, PrepareTemporalFields(CalendarDateAdd(y, w, C, f, G), I, []), O);
}
function AddDurationToOrSubtractDurationFromZonedDateTime(e, t, r, o) {
  var n = "subtract" === e ? -1 : 1,
    _ToTemporalDurationRe5 = ToTemporalDurationRecord(r),
    a = _ToTemporalDurationRe5.years,
    i = _ToTemporalDurationRe5.months,
    s = _ToTemporalDurationRe5.weeks,
    l = _ToTemporalDurationRe5.days,
    d = _ToTemporalDurationRe5.hours,
    m = _ToTemporalDurationRe5.minutes,
    c = _ToTemporalDurationRe5.seconds,
    h = _ToTemporalDurationRe5.milliseconds,
    u = _ToTemporalDurationRe5.microseconds,
    T = _ToTemporalDurationRe5.nanoseconds,
    f = GetOptionsObject(o),
    y = GetSlot(t, g),
    I = GetSlot(t, p);
  return CreateTemporalZonedDateTime(AddZonedDateTime(GetSlot(t, S), y, I, n * a, n * i, n * s, n * l, n * d, n * m, n * c, n * h, n * u, n * T, f), y, I);
}
function RoundNumberToIncrement(t, r, o) {
  if (r === Se) return t;
  var _divmod21 = divmod(t, r),
    n = _divmod21.quotient,
    a = _divmod21.remainder;
  if (a === Ie) return t;
  var i = a < Ie ? -1 : 1,
    s = abs(a * 2n),
    l = s === r,
    d = s > r;
  switch (o) {
    case "ceil":
      i > 0 && (n = n + BigInt(i));
      break;
    case "floor":
      i < 0 && (n = n + BigInt(i));
      break;
    case "expand":
      n = n + BigInt(i);
      break;
    case "trunc":
      break;
    case "halfCeil":
      (d || l && i > 0) && (n = n + BigInt(i));
      break;
    case "halfFloor":
      (d || l && i < 0) && (n = n + BigInt(i));
      break;
    case "halfExpand":
      (d || l) && (n = n + BigInt(i));
      break;
    case "halfTrunc":
      d && (n = n + BigInt(i));
      break;
    case "halfEven":
      (d || l && 1 === Number(abs(n) % 2n)) && (n = n + BigInt(i));
  }
  return n * r;
}
function RoundInstant(t, r, o, n) {
  var _NonNegativeBigIntDiv7 = NonNegativeBigIntDivmod(t, Ee),
    a = _NonNegativeBigIntDiv7.remainder;
  var i = t - a,
    s = RoundNumberToIncrement(a, BigInt(_e[o] * r), n);
  return i + s;
}
function RoundISODateTime(e, t, r, o, n, a, i, s, l, d, m, c) {
  var h = arguments.length > 12 && arguments[12] !== undefined ? arguments[12] : 864e11;
  var _RoundTime = RoundTime(o, n, a, i, s, l, d, m, c, h),
    u = _RoundTime.deltaDays,
    T = _RoundTime.hour,
    p = _RoundTime.minute,
    f = _RoundTime.second,
    y = _RoundTime.millisecond,
    I = _RoundTime.microsecond,
    S = _RoundTime.nanosecond,
    _BalanceISODate4 = BalanceISODate(e, t, r + u),
    g = _BalanceISODate4.year,
    w = _BalanceISODate4.month,
    D = _BalanceISODate4.day;
  return {
    year: g,
    month: w,
    day: D,
    hour: T,
    minute: p,
    second: f,
    millisecond: y,
    microsecond: I,
    nanosecond: S
  };
}
function RoundTime(t, r, o, n, a, i, s, l, d) {
  var m = arguments.length > 9 && arguments[9] !== undefined ? arguments[9] : 864e11;
  var c = Ie;
  switch (l) {
    case "day":
    case "hour":
      c = BigInt(t);
    case "minute":
      c = c * ge + BigInt(r);
    case "second":
      c = c * ge + BigInt(o);
    case "millisecond":
      c = c * De + BigInt(n);
    case "microsecond":
      c = c * De + BigInt(a);
    case "nanosecond":
      c = c * De + BigInt(i);
  }
  var h = "day" === l ? m : _e[l],
    u = RoundNumberToIncrement(c, BigInt(h * s), d),
    T = Number(u / BigInt(h));
  switch (l) {
    case "day":
      return {
        deltaDays: T,
        hour: 0,
        minute: 0,
        second: 0,
        millisecond: 0,
        microsecond: 0,
        nanosecond: 0
      };
    case "hour":
      return BalanceTime(T, 0, 0, 0, 0, 0);
    case "minute":
      return BalanceTime(t, T, 0, 0, 0, 0);
    case "second":
      return BalanceTime(t, r, T, 0, 0, 0);
    case "millisecond":
      return BalanceTime(t, r, o, T, 0, 0);
    case "microsecond":
      return BalanceTime(t, r, o, n, T, 0);
    case "nanosecond":
      return BalanceTime(t, r, o, n, a, T);
    default:
      throw new Error("Invalid unit ".concat(l));
  }
}
function DaysUntil(e, t) {
  return DifferenceISODate(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l), GetSlot(t, i), GetSlot(t, s), GetSlot(t, l), "day").days;
}
function MoveRelativeDate(e, t, r, o) {
  var n = CalendarDateAdd(e, t, r, void 0, o);
  return {
    relativeTo: n,
    days: DaysUntil(t, n)
  };
}
function MoveRelativeZonedDateTime(e, t, r, o, n) {
  var a = GetSlot(e, g),
    i = GetSlot(e, p);
  return CreateTemporalZonedDateTime(AddZonedDateTime(GetSlot(e, S), a, i, t, r, o, n, 0, 0, 0, 0, 0, 0), a, i);
}
function AdjustRoundedDurationDays(t, r, o, n, a, i, s, l, d, m, c, h, u, T) {
  var _AddDuration2, _BalanceDuration9;
  var f = t,
    y = r,
    I = o,
    w = n,
    D = a,
    G = i,
    v = s,
    C = l,
    O = d,
    b = m;
  if (!IsTemporalZonedDateTime(T) || "year" === h || "month" === h || "week" === h || "day" === h || "nanosecond" === h && 1 === c) return {
    years: f,
    months: y,
    weeks: I,
    days: w,
    hours: D,
    minutes: G,
    seconds: v,
    milliseconds: C,
    microseconds: O,
    nanoseconds: b
  };
  var E = TotalDurationNanoseconds(0, D, G, v, C, O, b, 0);
  var M = se(Number(E)),
    R = GetSlot(T, g),
    F = GetSlot(T, p),
    Y = AddZonedDateTime(GetSlot(T, S), R, F, f, y, I, w, 0, 0, 0, 0, 0, 0),
    P = AddZonedDateTime(new (GetIntrinsic("%Temporal.Instant%"))(Y), R, F, 0, 0, 0, M, 0, 0, 0, 0, 0, 0),
    Z = P - Y;
  return (E - Z) * BigInt(M) >= Ie && ((_AddDuration2 = AddDuration(f, y, I, w, 0, 0, 0, 0, 0, 0, 0, 0, 0, M, 0, 0, 0, 0, 0, 0, T), f = _AddDuration2.years, y = _AddDuration2.months, I = _AddDuration2.weeks, w = _AddDuration2.days), E = RoundInstant(E - Z, c, h, u), (_BalanceDuration9 = BalanceDuration(0, 0, 0, 0, 0, 0, Number(E), "hour"), D = _BalanceDuration9.hours, G = _BalanceDuration9.minutes, v = _BalanceDuration9.seconds, C = _BalanceDuration9.milliseconds, O = _BalanceDuration9.microseconds, b = _BalanceDuration9.nanoseconds)), {
    years: f,
    months: y,
    weeks: I,
    days: w,
    hours: D,
    minutes: G,
    seconds: v,
    milliseconds: C,
    microseconds: O,
    nanoseconds: b
  };
}
function RoundDuration(t, r, o, n, a, i, s, l, d, m, c, h, u, T) {
  var f = t,
    y = r,
    I = o,
    S = n,
    g = a,
    w = i,
    D = s,
    G = l,
    v = d,
    C = BigInt(m);
  var O = GetIntrinsic("%Temporal.Duration%");
  var b,
    E,
    M,
    R,
    F = T;
  if (F) {
    if (IsTemporalZonedDateTime(F)) E = F, F = ToTemporalDate(F);else if (!IsTemporalDate(F)) throw new TypeError("starting point must be PlainDate or ZonedDateTime");
    b = GetSlot(F, p);
  }
  if ("year" === h || "month" === h || "week" === h || "day" === h) {
    var _NanosecondsToDays3;
    var _t31, _r45, _o15;
    C = TotalDurationNanoseconds(0, g, w, D, G, v, m, 0), E && (_t31 = MoveRelativeZonedDateTime(E, f, y, I, S)), (_NanosecondsToDays3 = NanosecondsToDays(C, _t31), _r45 = _NanosecondsToDays3.days, C = _NanosecondsToDays3.nanoseconds, _o15 = _NanosecondsToDays3.dayLengthNs), M = BigInt(_o15), S += _r45, g = w = D = G = v = 0;
  }
  switch (h) {
    case "year":
      {
        if (!b) throw new RangeError("A starting point is required for years rounding");
        var _t32 = new O(f),
          _r46 = "string" != typeof b ? GetMethod(b, "dateAdd") : void 0,
          _o16 = CalendarDateAdd(b, F, _t32, void 0, _r46),
          _n12 = CalendarDateAdd(b, F, new O(f, y, I), void 0, _r46);
        F = _o16, S += DaysUntil(_o16, _n12);
        var _a8 = CalendarDateAdd(b, F, new O(0, 0, 0, S), void 0, _r46),
          _i12 = Te(null);
        _i12.largestUnit = "year";
        var _s4 = CalendarDateUntil(b, F, _a8, _i12).years;
        f += _s4;
        var _l5 = F;
        F = CalendarDateAdd(b, F, new O(_s4), void 0, _r46);
        S -= DaysUntil(_l5, F);
        var _d5 = new O(S < 0 ? -1 : 1);
        var _MoveRelativeDate6 = MoveRelativeDate(b, F, _d5, _r46),
          _m5 = _MoveRelativeDate6.days;
        _m5 = ae(_m5);
        var _h2 = BigInt(_m5) * M;
        C = _h2 * BigInt(f) + BigInt(S) * M + C;
        var _T2 = RoundNumberToIncrement(C, _h2 * BigInt(c), u);
        R = BigIntDivideToNumber(C, _h2), f = Number(_T2 / _h2), C = Ie, y = I = S = 0;
        break;
      }
    case "month":
      {
        if (!b) throw new RangeError("A starting point is required for months rounding");
        var _t33 = new O(f, y),
          _r47 = "string" != typeof b ? GetMethod(b, "dateAdd") : void 0,
          _o17 = CalendarDateAdd(b, F, _t33, void 0, _r47),
          _n13 = CalendarDateAdd(b, F, new O(f, y, I), void 0, _r47);
        F = _o17, S += DaysUntil(_o17, _n13);
        var _a9 = se(S),
          _i13 = new O(0, S < 0 ? -1 : 1);
        var _s5;
        for (_MoveRelativeDate7 = MoveRelativeDate(b, F, _i13, _r47), F = _MoveRelativeDate7.relativeTo, _s5 = _MoveRelativeDate7.days, _MoveRelativeDate7; ae(S) >= ae(_s5);) {
          var _MoveRelativeDate7, _MoveRelativeDate8;
          y += _a9, S -= _s5, (_MoveRelativeDate8 = MoveRelativeDate(b, F, _i13, _r47), F = _MoveRelativeDate8.relativeTo, _s5 = _MoveRelativeDate8.days);
        }
        _s5 = ae(_s5);
        var _l6 = BigInt(_s5) * M;
        C = _l6 * BigInt(y) + BigInt(S) * M + C;
        var _d6 = RoundNumberToIncrement(C, _l6 * BigInt(c), u);
        R = BigIntDivideToNumber(C, _l6), y = Number(_d6 / _l6), C = Ie, I = S = 0;
        break;
      }
    case "week":
      {
        if (!b) throw new RangeError("A starting point is required for weeks rounding");
        var _t34 = se(S),
          _r48 = new O(0, 0, S < 0 ? -1 : 1),
          _o18 = "string" != typeof b ? GetMethod(b, "dateAdd") : void 0;
        var _n14;
        for (_MoveRelativeDate9 = MoveRelativeDate(b, F, _r48, _o18), F = _MoveRelativeDate9.relativeTo, _n14 = _MoveRelativeDate9.days, _MoveRelativeDate9; ae(S) >= ae(_n14);) {
          var _MoveRelativeDate9, _MoveRelativeDate10;
          I += _t34, S -= _n14, (_MoveRelativeDate10 = MoveRelativeDate(b, F, _r48, _o18), F = _MoveRelativeDate10.relativeTo, _n14 = _MoveRelativeDate10.days);
        }
        _n14 = ae(_n14);
        var _a10 = BigInt(_n14) * M;
        C = _a10 * BigInt(I) + BigInt(S) * M + C;
        var _i14 = RoundNumberToIncrement(C, _a10 * BigInt(c), u);
        R = BigIntDivideToNumber(C, _a10), I = Number(_i14 / _a10), C = Ie, S = 0;
        break;
      }
    case "day":
      {
        var _t35 = M;
        C = _t35 * BigInt(S) + C;
        var _r49 = RoundNumberToIncrement(C, _t35 * BigInt(c), u);
        R = BigIntDivideToNumber(C, _t35), S = Number(_r49 / _t35), C = Ie;
        break;
      }
    case "hour":
      {
        var _t36 = 36e11;
        var _r50 = BigInt(g) * 3600000000000n;
        _r50 = _r50 + BigInt(w) * 60000000000n, _r50 = _r50 + BigInt(D) * ve, _r50 = _r50 + BigInt(G) * Ge, _r50 = _r50 + BigInt(v) * De, _r50 = _r50 + C, R = BigIntDivideToNumber(_r50, BigInt(_t36));
        var _o19 = RoundNumberToIncrement(_r50, BigInt(_t36 * c), u);
        g = Number(_o19 / BigInt(_t36)), C = Ie, w = D = G = v = 0;
        break;
      }
    case "minute":
      {
        var _t37 = 6e10;
        var _r51 = BigInt(w) * 60000000000n;
        _r51 = _r51 + BigInt(D) * ve, _r51 = _r51 + BigInt(G) * Ge, _r51 = _r51 + BigInt(v) * De, _r51 = _r51 + C, R = BigIntDivideToNumber(_r51, BigInt(_t37));
        var _o20 = RoundNumberToIncrement(_r51, BigInt(_t37 * c), u);
        w = Number(_o20 / BigInt(_t37)), C = Ie, D = G = v = 0;
        break;
      }
    case "second":
      {
        var _t38 = 1e9;
        var _r52 = BigInt(D) * ve;
        _r52 = _r52 + BigInt(G) * Ge, _r52 = _r52 + BigInt(v) * De, _r52 = _r52 + C, R = BigIntDivideToNumber(_r52, BigInt(_t38));
        var _o21 = RoundNumberToIncrement(_r52, BigInt(_t38 * c), u);
        D = Number(_o21 / BigInt(_t38)), C = Ie, G = v = 0;
        break;
      }
    case "millisecond":
      {
        var _t39 = 1e6;
        var _r53 = BigInt(G) * Ge;
        _r53 = _r53 + BigInt(v) * De, _r53 = _r53 + C, R = BigIntDivideToNumber(_r53, BigInt(_t39));
        var _o22 = RoundNumberToIncrement(_r53, BigInt(_t39 * c), u);
        G = Number(_o22 / BigInt(_t39)), C = Ie, v = 0;
        break;
      }
    case "microsecond":
      {
        var _t40 = 1e3;
        var _r54 = BigInt(v) * De;
        _r54 = _r54 + C, R = BigIntDivideToNumber(_r54, BigInt(_t40));
        var _o23 = RoundNumberToIncrement(_r54, BigInt(_t40 * c), u);
        v = Number(_o23 / BigInt(_t40)), C = Ie;
        break;
      }
    case "nanosecond":
      R = Number(C), C = RoundNumberToIncrement(BigInt(C), BigInt(c), u);
  }
  return {
    years: f,
    months: y,
    weeks: I,
    days: S,
    hours: g,
    minutes: w,
    seconds: D,
    milliseconds: G,
    microseconds: v,
    nanoseconds: Number(C),
    total: R
  };
}
function CompareISODate(e, t, r, o, n, a) {
  for (var _i15 = 0, _arr5 = [[e, o], [t, n], [r, a]]; _i15 < _arr5.length; _i15++) {
    var _arr5$_i = _slicedToArray(_arr5[_i15], 2),
      _i16 = _arr5$_i[0],
      _s6 = _arr5$_i[1];
    if (_i16 !== _s6) return ComparisonResult(_i16 - _s6);
  }
  return 0;
}
function NonNegativeBigIntDivmod(t, r) {
  var _divmod22 = divmod(t, r),
    o = _divmod22.quotient,
    n = _divmod22.remainder;
  return n < Ie && (o = o - Se, n = n + r), {
    quotient: o,
    remainder: n
  };
}
function BigIntFloorDiv(t, r) {
  var _divmod23 = divmod(t, r),
    o = _divmod23.quotient,
    n = _divmod23.remainder;
  return isZero(n) || !isNegativeJSBI(t) == !isNegativeJSBI(r) ? o : o - Se;
}
function BigIntDivideToNumber(t, r) {
  var _divmod24 = divmod(t, r),
    o = _divmod24.quotient,
    n = _divmod24.remainder;
  return Number(o) + Number(n) / Number(r);
}
function ToBigIntExternal(e) {
  var t = ToBigInt(e);
  return void 0 !== globalThis.BigInt ? globalThis.BigInt(t.toString(10)) : t;
}
function ToBigInt(t) {
  var r = t;
  if ("object" == _typeof(t)) {
    var _e32 = t[Symbol.toPrimitive];
    _e32 && "function" == typeof _e32 && (r = fe(_e32, t, ["number"]));
  }
  if ("number" == typeof r) throw new TypeError("cannot convert number to bigint");
  return "bigint" == typeof r ? BigInt(r.toString(10)) : BigInt(r);
}
var Ve = function () {
  var t = BigInt(Date.now() % 1e6);
  return function () {
    var r = BigInt(Date.now()),
      o = r * Ge + t;
    return t = r % Ge, o > Re ? Re : o < Me ? Me : o;
  };
}();
function DefaultTimeZone() {
  return new re().resolvedOptions().timeZone;
}
function ComparisonResult(e) {
  return e < 0 ? -1 : e > 0 ? 1 : e;
}
function GetOptionsObject(e) {
  if (void 0 === e) return Te(null);
  if (IsObject(e) && null !== e) return e;
  throw new TypeError("Options parameter must be an object, not " + (null === e ? "null" : "" + _typeof(e)));
}
function CreateOnePropObject(e, t) {
  var r = Te(null);
  return r[e] = t, r;
}
function CopyOptions(e) {
  var t = Te(null);
  return CopyDataProperties(t, GetOptionsObject(e), []), t;
}
function GetOption(e, t, r, o) {
  var n = e[t];
  if (void 0 !== n) {
    if (n = ToString(n), !r.includes(n)) throw new RangeError("".concat(t, " must be one of ").concat(r.join(", "), ", not ").concat(n));
    return n;
  }
  return o;
}
function IsBuiltinCalendar(e) {
  return je.includes(ASCIILowercase(e));
}
function ASCIILowercase(e) {
  return e.replace(/[A-Z]/g, function (e) {
    var t = e.charCodeAt(0);
    return String.fromCharCode(t + 32);
  });
}
var ze = new RegExp("^".concat(W.source, "$"));
function bisect(t, r, o) {
  var n = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : t(r);
  var a = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : t(o);
  var i = BigInt(r),
    s = BigInt(o),
    l = n,
    d = a;
  for (; s - i > Se;) {
    var _r55 = (i + s) / 2n,
      _o24 = t(_r55);
    if (_o24 === l) i = _r55, l = _o24;else {
      if (_o24 !== d) throw new Error("invalid state in bisection ".concat(l, " - ").concat(_o24, " - ").concat(d));
      s = _r55, d = _o24;
    }
  }
  return s;
}
var _e = {
    hour: 36e11,
    minute: 6e10,
    second: 1e9,
    millisecond: 1e6,
    microsecond: 1e3,
    nanosecond: 1
  },
  Je = Symbol("date"),
  Ke = Symbol("ym"),
  Xe = Symbol("md"),
  Qe = Symbol("time"),
  et = Symbol("datetime"),
  tt = Symbol("instant"),
  rt = Symbol("original"),
  ot = Symbol("timezone"),
  nt = Symbol("calendar-id"),
  at = Symbol("locale"),
  it = Symbol("options"),
  descriptor = function descriptor(e) {
    return {
      value: e,
      enumerable: !0,
      writable: !1,
      configurable: !0
    };
  },
  st = globalThis.Intl.DateTimeFormat,
  lt = Object.assign,
  dt = Object.prototype.hasOwnProperty,
  mt = Reflect.apply;
function getPropLazy(e, t) {
  var r = e[t];
  return "function" == typeof r && (r = new st(e[at], r(e[it])), e[t] = r), r;
}
function DateTimeFormatImpl(e) {
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
  if (!(this instanceof DateTimeFormatImpl)) return new DateTimeFormatImpl(e, t);
  var r = void 0 !== t,
    o = r ? lt({}, t) : {},
    n = new st(e, o),
    a = n.resolvedOptions();
  if (r) {
    var _e33 = lt({}, a);
    for (var _t41 in _e33) mt(dt, o, [_t41]) || delete _e33[_t41];
    this[it] = _e33;
  } else this[it] = o;
  this[at] = a.locale, this[rt] = n, this[ot] = a.timeZone, this[nt] = a.calendar, this[Je] = dateAmend, this[Ke] = yearMonthAmend, this[Xe] = monthDayAmend, this[Qe] = timeAmend, this[et] = datetimeAmend, this[tt] = instantAmend;
}
Object.defineProperty(DateTimeFormatImpl, "name", {
  writable: !0,
  value: "DateTimeFormat"
}), DateTimeFormatImpl.supportedLocalesOf = function (e, t) {
  return st.supportedLocalesOf(e, t);
};
var ct = {
  resolvedOptions: descriptor(function resolvedOptions() {
    return this[rt].resolvedOptions();
  }),
  format: descriptor(function format(e) {
    var _this$rt;
    var _extractOverrides = extractOverrides(e, this),
      r = _extractOverrides.instant,
      o = _extractOverrides.formatter;
    if (r && o) return o.format(r.epochMilliseconds);
    for (var _len2 = arguments.length, t = new Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
      t[_key2 - 1] = arguments[_key2];
    }
    return (_this$rt = this[rt]).format.apply(_this$rt, [e].concat(t));
  }),
  formatRange: descriptor(function formatRange(e, t) {
    if (isTemporalObject(e) || isTemporalObject(t)) {
      if (!sameTemporalType(e, t)) throw new TypeError("Intl.DateTimeFormat.formatRange accepts two values of the same type");
      var _extractOverrides2 = extractOverrides(e, this),
        _r56 = _extractOverrides2.instant,
        _o25 = _extractOverrides2.formatter,
        _extractOverrides3 = extractOverrides(t, this),
        _n15 = _extractOverrides3.instant,
        _a11 = _extractOverrides3.formatter;
      if (_r56 && _n15 && _o25 && _a11 && _o25 === _a11) return _o25.formatRange(_r56.epochMilliseconds, _n15.epochMilliseconds);
    }
    return this[rt].formatRange(e, t);
  })
};
"formatToParts" in st.prototype && (ct.formatToParts = descriptor(function formatToParts(e) {
  var _this$rt2;
  var _extractOverrides4 = extractOverrides(e, this),
    r = _extractOverrides4.instant,
    o = _extractOverrides4.formatter;
  if (r && o) return o.formatToParts(r.epochMilliseconds);
  for (var _len3 = arguments.length, t = new Array(_len3 > 1 ? _len3 - 1 : 0), _key3 = 1; _key3 < _len3; _key3++) {
    t[_key3 - 1] = arguments[_key3];
  }
  return (_this$rt2 = this[rt]).formatToParts.apply(_this$rt2, [e].concat(t));
})), "formatRangeToParts" in st.prototype && (ct.formatRangeToParts = descriptor(function formatRangeToParts(e, t) {
  if (isTemporalObject(e) || isTemporalObject(t)) {
    if (!sameTemporalType(e, t)) throw new TypeError("Intl.DateTimeFormat.formatRangeToParts accepts two values of the same type");
    var _extractOverrides5 = extractOverrides(e, this),
      _r57 = _extractOverrides5.instant,
      _o26 = _extractOverrides5.formatter,
      _extractOverrides6 = extractOverrides(t, this),
      _n16 = _extractOverrides6.instant,
      _a12 = _extractOverrides6.formatter;
    if (_r57 && _n16 && _o26 && _a12 && _o26 === _a12) return _o26.formatRangeToParts(_r57.epochMilliseconds, _n16.epochMilliseconds);
  }
  return this[rt].formatRangeToParts(e, t);
})), DateTimeFormatImpl.prototype = Object.create(st.prototype, ct), Object.defineProperty(DateTimeFormatImpl, "prototype", {
  writable: !1,
  enumerable: !1,
  configurable: !1
});
var ht = DateTimeFormatImpl;
function amend() {
  var e = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
  var r = lt({}, e);
  for (var _i17 = 0, _arr6 = ["year", "month", "day", "hour", "minute", "second", "weekday", "dayPeriod", "timeZoneName", "dateStyle", "timeStyle"]; _i17 < _arr6.length; _i17++) {
    var _e34 = _arr6[_i17];
    r[_e34] = _e34 in t ? t[_e34] : r[_e34], !1 !== r[_e34] && void 0 !== r[_e34] || delete r[_e34];
  }
  return r;
}
function timeAmend(e) {
  var t = amend(e, {
    year: !1,
    month: !1,
    day: !1,
    weekday: !1,
    timeZoneName: !1,
    dateStyle: !1
  });
  return hasTimeOptions(t) || (t = lt({}, t, {
    hour: "numeric",
    minute: "numeric",
    second: "numeric"
  })), t;
}
function yearMonthAmend(e) {
  var t = amend(e, {
    day: !1,
    hour: !1,
    minute: !1,
    second: !1,
    weekday: !1,
    dayPeriod: !1,
    timeZoneName: !1,
    dateStyle: !1,
    timeStyle: !1
  });
  return "year" in t || "month" in t || (t = lt(t, {
    year: "numeric",
    month: "numeric"
  })), t;
}
function monthDayAmend(e) {
  var t = amend(e, {
    year: !1,
    hour: !1,
    minute: !1,
    second: !1,
    weekday: !1,
    dayPeriod: !1,
    timeZoneName: !1,
    dateStyle: !1,
    timeStyle: !1
  });
  return "month" in t || "day" in t || (t = lt({}, t, {
    month: "numeric",
    day: "numeric"
  })), t;
}
function dateAmend(e) {
  var t = amend(e, {
    hour: !1,
    minute: !1,
    second: !1,
    dayPeriod: !1,
    timeZoneName: !1,
    timeStyle: !1
  });
  return hasDateOptions(t) || (t = lt({}, t, {
    year: "numeric",
    month: "numeric",
    day: "numeric"
  })), t;
}
function datetimeAmend(e) {
  var t = amend(e, {
    timeZoneName: !1
  });
  return hasTimeOptions(t) || hasDateOptions(t) || (t = lt({}, t, {
    year: "numeric",
    month: "numeric",
    day: "numeric",
    hour: "numeric",
    minute: "numeric",
    second: "numeric"
  })), t;
}
function instantAmend(e) {
  var t = e;
  return hasTimeOptions(t) || hasDateOptions(t) || (t = lt({}, t, {
    year: "numeric",
    month: "numeric",
    day: "numeric",
    hour: "numeric",
    minute: "numeric",
    second: "numeric"
  })), t;
}
function hasDateOptions(e) {
  return "year" in e || "month" in e || "day" in e || "weekday" in e || "dateStyle" in e;
}
function hasTimeOptions(e) {
  return "hour" in e || "minute" in e || "second" in e || "timeStyle" in e || "dayPeriod" in e;
}
function isTemporalObject(e) {
  return IsTemporalDate(e) || IsTemporalTime(e) || IsTemporalDateTime(e) || IsTemporalZonedDateTime(e) || IsTemporalYearMonth(e) || IsTemporalMonthDay(e) || IsTemporalInstant(e);
}
function sameTemporalType(e, t) {
  return !(!isTemporalObject(e) || !isTemporalObject(t)) && !(IsTemporalTime(e) && !IsTemporalTime(t)) && !(IsTemporalDate(e) && !IsTemporalDate(t)) && !(IsTemporalDateTime(e) && !IsTemporalDateTime(t)) && !(IsTemporalZonedDateTime(e) && !IsTemporalZonedDateTime(t)) && !(IsTemporalYearMonth(e) && !IsTemporalYearMonth(t)) && !(IsTemporalMonthDay(e) && !IsTemporalMonthDay(t)) && !(IsTemporalInstant(e) && !IsTemporalInstant(t));
}
function extractOverrides(e, t) {
  var r = GetIntrinsic("%Temporal.PlainDateTime%");
  if (IsTemporalTime(e)) {
    var _o27 = new r(1970, 1, 1, GetSlot(e, d), GetSlot(e, m), GetSlot(e, c), GetSlot(e, h), GetSlot(e, u), GetSlot(e, T), t[nt]);
    return {
      instant: GetInstantFor(t[ot], _o27, "compatible"),
      formatter: getPropLazy(t, Qe)
    };
  }
  if (IsTemporalYearMonth(e)) {
    var _o28 = GetSlot(e, i),
      _n17 = GetSlot(e, s),
      _a13 = GetSlot(e, l),
      _d7 = ToTemporalCalendarIdentifier(GetSlot(e, p));
    if (_d7 !== t[nt]) throw new RangeError("cannot format PlainYearMonth with calendar ".concat(_d7, " in locale with calendar ").concat(t[nt]));
    var _m6 = new r(_o28, _n17, _a13, 12, 0, 0, 0, 0, 0, _d7);
    return {
      instant: GetInstantFor(t[ot], _m6, "compatible"),
      formatter: getPropLazy(t, Ke)
    };
  }
  if (IsTemporalMonthDay(e)) {
    var _o29 = GetSlot(e, i),
      _n18 = GetSlot(e, s),
      _a14 = GetSlot(e, l),
      _d8 = ToTemporalCalendarIdentifier(GetSlot(e, p));
    if (_d8 !== t[nt]) throw new RangeError("cannot format PlainMonthDay with calendar ".concat(_d8, " in locale with calendar ").concat(t[nt]));
    var _m7 = new r(_o29, _n18, _a14, 12, 0, 0, 0, 0, 0, _d8);
    return {
      instant: GetInstantFor(t[ot], _m7, "compatible"),
      formatter: getPropLazy(t, Xe)
    };
  }
  if (IsTemporalDate(e)) {
    var _o30 = GetSlot(e, i),
      _n19 = GetSlot(e, s),
      _a15 = GetSlot(e, l),
      _d9 = ToTemporalCalendarIdentifier(GetSlot(e, p));
    if ("iso8601" !== _d9 && _d9 !== t[nt]) throw new RangeError("cannot format PlainDate with calendar ".concat(_d9, " in locale with calendar ").concat(t[nt]));
    var _m8 = new r(_o30, _n19, _a15, 12, 0, 0, 0, 0, 0, t[nt]);
    return {
      instant: GetInstantFor(t[ot], _m8, "compatible"),
      formatter: getPropLazy(t, Je)
    };
  }
  if (IsTemporalDateTime(e)) {
    var _o31 = GetSlot(e, i),
      _n20 = GetSlot(e, s),
      _a16 = GetSlot(e, l),
      _f5 = GetSlot(e, d),
      _y = GetSlot(e, m),
      _I3 = GetSlot(e, c),
      _S3 = GetSlot(e, h),
      _g2 = GetSlot(e, u),
      _w = GetSlot(e, T),
      _D = ToTemporalCalendarIdentifier(GetSlot(e, p));
    if ("iso8601" !== _D && _D !== t[nt]) throw new RangeError("cannot format PlainDateTime with calendar ".concat(_D, " in locale with calendar ").concat(t[nt]));
    var _G = e;
    return "iso8601" === _D && (_G = new r(_o31, _n20, _a16, _f5, _y, _I3, _S3, _g2, _w, t[nt])), {
      instant: GetInstantFor(t[ot], _G, "compatible"),
      formatter: getPropLazy(t, et)
    };
  }
  if (IsTemporalZonedDateTime(e)) throw new TypeError("Temporal.ZonedDateTime not supported in DateTimeFormat methods. Use toLocaleString() instead.");
  return IsTemporalInstant(e) ? {
    instant: e,
    formatter: getPropLazy(t, tt)
  } : {};
}
var ut = Object.freeze({
  __proto__: null,
  DateTimeFormat: ht
});
var Instant = /*#__PURE__*/function () {
  function Instant(e) {
    _classCallCheck(this, Instant);
    if (arguments.length < 1) throw new TypeError("missing argument: epochNanoseconds is required");
    var t = ToBigInt(e);
    ValidateEpochNanoseconds(t), N(this), SetSlot(this, n, t);
  }
  _createClass(Instant, [{
    key: "epochSeconds",
    get: function get() {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      var t = GetSlot(this, n);
      return Number(BigIntFloorDiv(t, ve));
    }
  }, {
    key: "epochMilliseconds",
    get: function get() {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      var t = BigInt(GetSlot(this, n));
      return Number(BigIntFloorDiv(t, Ge));
    }
  }, {
    key: "epochMicroseconds",
    get: function get() {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return ToBigIntExternal(BigIntFloorDiv(BigInt(GetSlot(this, n)), De));
    }
  }, {
    key: "epochNanoseconds",
    get: function get() {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return ToBigIntExternal(BigInt(GetSlot(this, n)));
    }
  }, {
    key: "add",
    value: function add(e) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromInstant("add", this, e);
    }
  }, {
    key: "subtract",
    value: function subtract(e) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromInstant("subtract", this, e);
    }
  }, {
    key: "until",
    value: function until(e, t) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalInstant("until", this, e, t);
    }
  }, {
    key: "since",
    value: function since(e, t) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalInstant("since", this, e, t);
    }
  }, {
    key: "round",
    value: function round(e) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      if (void 0 === e) throw new TypeError("options parameter is required");
      var t = "string" == typeof e ? CreateOnePropObject("smallestUnit", e) : GetOptionsObject(e),
        r = ToTemporalRoundingIncrement(t),
        o = ToTemporalRoundingMode(t, "halfExpand"),
        a = GetTemporalUnit(t, "smallestUnit", "time", He);
      ValidateTemporalRoundingIncrement(r, {
        hour: 24,
        minute: 1440,
        second: 86400,
        millisecond: 864e5,
        microsecond: 864e8,
        nanosecond: 864e11
      }[a], !0);
      var i = RoundInstant(GetSlot(this, n), r, a, o);
      return new Instant(i);
    }
  }, {
    key: "equals",
    value: function equals(t) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      var r = ToTemporalInstant(t),
        o = GetSlot(this, n),
        a = GetSlot(r, n);
      return BigInt(o) === BigInt(a);
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      var t = GetOptionsObject(e),
        r = ToFractionalSecondDigits(t),
        o = ToTemporalRoundingMode(t, "trunc"),
        a = GetTemporalUnit(t, "smallestUnit", "time", void 0);
      if ("hour" === a) throw new RangeError('smallestUnit must be a time unit other than "hour"');
      var i = t.timeZone;
      void 0 !== i && (i = ToTemporalTimeZoneSlotValue(i));
      var _ToSecondsStringPreci = ToSecondsStringPrecisionRecord(a, r),
        s = _ToSecondsStringPreci.precision,
        l = _ToSecondsStringPreci.unit,
        d = _ToSecondsStringPreci.increment,
        m = RoundInstant(GetSlot(this, n), d, l, o);
      return TemporalInstantToString(new Instant(m), i, s);
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return TemporalInstantToString(this, void 0, "auto");
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      return new ht(e, t).format(this);
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use compare() or equals() to compare Temporal.Instant");
    }
  }, {
    key: "toZonedDateTime",
    value: function toZonedDateTime(e) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid argument in toZonedDateTime");
      var t = e.calendar;
      if (void 0 === t) throw new TypeError("missing calendar property in toZonedDateTime");
      var r = ToTemporalCalendarSlotValue(t),
        o = e.timeZone;
      if (void 0 === o) throw new TypeError("missing timeZone property in toZonedDateTime");
      var a = ToTemporalTimeZoneSlotValue(o);
      return CreateTemporalZonedDateTime(GetSlot(this, n), a, r);
    }
  }, {
    key: "toZonedDateTimeISO",
    value: function toZonedDateTimeISO(e) {
      if (!IsTemporalInstant(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalTimeZoneSlotValue(e);
      return CreateTemporalZonedDateTime(GetSlot(this, n), t, "iso8601");
    }
  }], [{
    key: "fromEpochSeconds",
    value: function fromEpochSeconds(t) {
      var r = ToNumber(t),
        o = BigInt(r) * ve;
      return ValidateEpochNanoseconds(o), new Instant(o);
    }
  }, {
    key: "fromEpochMilliseconds",
    value: function fromEpochMilliseconds(t) {
      var r = ToNumber(t),
        o = BigInt(r) * Ge;
      return ValidateEpochNanoseconds(o), new Instant(o);
    }
  }, {
    key: "fromEpochMicroseconds",
    value: function fromEpochMicroseconds(t) {
      var r = ToBigInt(t),
        o = r * De;
      return ValidateEpochNanoseconds(o), new Instant(o);
    }
  }, {
    key: "fromEpochNanoseconds",
    value: function fromEpochNanoseconds(e) {
      var t = ToBigInt(e);
      return ValidateEpochNanoseconds(t), new Instant(t);
    }
  }, {
    key: "from",
    value: function from(e) {
      return IsTemporalInstant(e) ? new Instant(GetSlot(e, n)) : ToTemporalInstant(e);
    }
  }, {
    key: "compare",
    value: function compare(t, r) {
      var o = ToTemporalInstant(t),
        a = ToTemporalInstant(r),
        i = GetSlot(o, n),
        s = GetSlot(a, n);
      return i < s ? -1 : i > s ? 1 : 0;
    }
  }]);
  return Instant;
}();
MakeIntrinsicClass(Instant, "Temporal.Instant");
var Tt = Array.prototype.includes,
  pt = Array.prototype.push,
  ft = globalThis.Intl.DateTimeFormat,
  yt = Array.prototype.sort,
  It = Math.abs,
  St = Math.floor,
  gt = Object.create,
  wt = Object.entries,
  Dt = Set,
  Gt = Reflect.ownKeys,
  vt = Set.prototype.add,
  Ct = Set.prototype.values,
  Ot = {};
var Calendar = /*#__PURE__*/function () {
  function Calendar(e) {
    _classCallCheck(this, Calendar);
    if (arguments.length < 1) throw new RangeError("missing argument: id is required");
    var t = ToString(e);
    if (!IsBuiltinCalendar(t)) throw new RangeError("invalid calendar identifier ".concat(t));
    N(this), SetSlot(this, F, ASCIILowercase(t));
  }
  _createClass(Calendar, [{
    key: "id",
    get: function get() {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, F);
    }
  }, {
    key: "dateFromFields",
    value: function dateFromFields(e, t) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid fields");
      var r = GetOptionsObject(t),
        o = GetSlot(this, F);
      return Ot[o].dateFromFields(e, r, o);
    }
  }, {
    key: "yearMonthFromFields",
    value: function yearMonthFromFields(e, t) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid fields");
      var r = GetOptionsObject(t),
        o = GetSlot(this, F);
      return Ot[o].yearMonthFromFields(e, r, o);
    }
  }, {
    key: "monthDayFromFields",
    value: function monthDayFromFields(e, t) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid fields");
      var r = GetOptionsObject(t),
        o = GetSlot(this, F);
      return Ot[o].monthDayFromFields(e, r, o);
    }
  }, {
    key: "fields",
    value: function fields(e) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var t = [],
        r = new Set(["year", "month", "monthCode", "day", "hour", "minute", "second", "millisecond", "microsecond", "nanosecond"]);
      var _iterator15 = _createForOfIteratorHelper(e),
        _step15;
      try {
        for (_iterator15.s(); !(_step15 = _iterator15.n()).done;) {
          var _o32 = _step15.value;
          if ("string" != typeof _o32) throw new TypeError("invalid fields");
          if (!r.has(_o32)) throw new RangeError("invalid field name ".concat(_o32));
          r["delete"](_o32), pt.call(t, _o32);
        }
      } catch (err) {
        _iterator15.e(err);
      } finally {
        _iterator15.f();
      }
      return Ot[GetSlot(this, F)].fields(t);
    }
  }, {
    key: "mergeFields",
    value: function mergeFields(e, t) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var r = ToObject(e),
        o = gt(null);
      CopyDataProperties(o, r, [], [void 0]);
      var n = ToObject(t),
        a = gt(null);
      CopyDataProperties(a, n, [], [void 0]);
      var i = Gt(a),
        s = Ot[GetSlot(this, F)].fieldKeysToIgnore(i),
        l = gt(null),
        d = Gt(o);
      var _iterator16 = _createForOfIteratorHelper(d),
        _step16;
      try {
        for (_iterator16.s(); !(_step16 = _iterator16.n()).done;) {
          var _e35 = _step16.value;
          var _t42 = void 0;
          _t42 = Call(Tt, s, [_e35]) ? a[_e35] : o[_e35], void 0 !== _t42 && (l[_e35] = _t42);
        }
      } catch (err) {
        _iterator16.e(err);
      } finally {
        _iterator16.f();
      }
      return CopyDataProperties(l, a, []), l;
    }
  }, {
    key: "dateAdd",
    value: function dateAdd(e, t, r) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var o = ToTemporalDate(e),
        n = ToTemporalDuration(t),
        a = ToTemporalOverflow(GetOptionsObject(r)),
        _BalanceDuration10 = BalanceDuration(GetSlot(n, v), GetSlot(n, C), GetSlot(n, O), GetSlot(n, b), GetSlot(n, E), GetSlot(n, M), GetSlot(n, R), "day"),
        i = _BalanceDuration10.days,
        s = GetSlot(this, F);
      return Ot[s].dateAdd(o, GetSlot(n, w), GetSlot(n, D), GetSlot(n, G), i, a, s);
    }
  }, {
    key: "dateUntil",
    value: function dateUntil(e, t, r) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var o = ToTemporalDate(e),
        n = ToTemporalDate(t);
      var a = GetTemporalUnit(GetOptionsObject(r), "largestUnit", "date", "auto");
      "auto" === a && (a = "day");
      var _Ot$GetSlot$dateUntil = Ot[GetSlot(this, F)].dateUntil(o, n, a),
        i = _Ot$GetSlot$dateUntil.years,
        s = _Ot$GetSlot$dateUntil.months,
        l = _Ot$GetSlot$dateUntil.weeks,
        d = _Ot$GetSlot$dateUntil.days;
      return new (GetIntrinsic("%Temporal.Duration%"))(i, s, l, d, 0, 0, 0, 0, 0, 0);
    }
  }, {
    key: "year",
    value: function year(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].year(t);
    }
  }, {
    key: "month",
    value: function month(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      if (IsTemporalMonthDay(t)) throw new TypeError("use monthCode on PlainMonthDay instead");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].month(t);
    }
  }, {
    key: "monthCode",
    value: function monthCode(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || IsTemporalMonthDay(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].monthCode(t);
    }
  }, {
    key: "day",
    value: function day(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalMonthDay(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].day(t);
    }
  }, {
    key: "era",
    value: function era(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].era(t);
    }
  }, {
    key: "eraYear",
    value: function eraYear(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].eraYear(t);
    }
  }, {
    key: "dayOfWeek",
    value: function dayOfWeek(e) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e);
      return Ot[GetSlot(this, F)].dayOfWeek(t);
    }
  }, {
    key: "dayOfYear",
    value: function dayOfYear(e) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e);
      return Ot[GetSlot(this, F)].dayOfYear(t);
    }
  }, {
    key: "weekOfYear",
    value: function weekOfYear(e) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e);
      return Ot[GetSlot(this, F)].weekOfYear(t);
    }
  }, {
    key: "yearOfWeek",
    value: function yearOfWeek(e) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e);
      return Ot[GetSlot(this, F)].yearOfWeek(t);
    }
  }, {
    key: "daysInWeek",
    value: function daysInWeek(e) {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e);
      return Ot[GetSlot(this, F)].daysInWeek(t);
    }
  }, {
    key: "daysInMonth",
    value: function daysInMonth(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].daysInMonth(t);
    }
  }, {
    key: "daysInYear",
    value: function daysInYear(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].daysInYear(t);
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].monthsInYear(t);
    }
  }, {
    key: "inLeapYear",
    value: function inLeapYear(e) {
      var t = e;
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return IsTemporalYearMonth(t) || (t = ToTemporalDate(t)), Ot[GetSlot(this, F)].inLeapYear(t);
    }
  }, {
    key: "toString",
    value: function toString() {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, F);
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalCalendar(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, F);
    }
  }], [{
    key: "from",
    value: function from(e) {
      return ToTemporalCalendarObject(ToTemporalCalendarSlotValue(e));
    }
  }]);
  return Calendar;
}();
function monthCodeNumberPart(e) {
  if (!e.startsWith("M")) throw new RangeError("Invalid month code: ".concat(e, ".  Month codes must start with M."));
  var t = +e.slice(1);
  if (isNaN(t)) throw new RangeError("Invalid month code: ".concat(e));
  return t;
}
function buildMonthCode(e) {
  var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : !1;
  return "M".concat(e.toString().padStart(2, "0")).concat(t ? "L" : "");
}
function resolveNonLunisolarMonth(e, t) {
  var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 12;
  var o = e.month,
    n = e.monthCode;
  if (void 0 === n) {
    if (void 0 === o) throw new TypeError("Either month or monthCode are required");
    "reject" === t && RejectToRange(o, 1, r), "constrain" === t && (o = ConstrainToRange(o, 1, r)), n = buildMonthCode(o);
  } else {
    var _e36 = monthCodeNumberPart(n);
    if (void 0 !== o && o !== _e36) throw new RangeError("monthCode ".concat(n, " and month ").concat(o, " must match if both are present"));
    if (n !== buildMonthCode(_e36)) throw new RangeError("Invalid month code: ".concat(n));
    if (o = _e36, o < 1 || o > r) throw new RangeError("Invalid monthCode: ".concat(n));
  }
  return _objectSpread2(_objectSpread2({}, e), {}, {
    month: o,
    monthCode: n
  });
}
MakeIntrinsicClass(Calendar, "Temporal.Calendar"), DefineIntrinsic("Temporal.Calendar.from", Calendar.from), DefineIntrinsic("Temporal.Calendar.prototype.dateAdd", Calendar.prototype.dateAdd), DefineIntrinsic("Temporal.Calendar.prototype.dateFromFields", Calendar.prototype.dateFromFields), DefineIntrinsic("Temporal.Calendar.prototype.dateUntil", Calendar.prototype.dateUntil), DefineIntrinsic("Temporal.Calendar.prototype.day", Calendar.prototype.day), DefineIntrinsic("Temporal.Calendar.prototype.dayOfWeek", Calendar.prototype.dayOfWeek), DefineIntrinsic("Temporal.Calendar.prototype.dayOfYear", Calendar.prototype.dayOfYear), DefineIntrinsic("Temporal.Calendar.prototype.daysInMonth", Calendar.prototype.daysInMonth), DefineIntrinsic("Temporal.Calendar.prototype.daysInWeek", Calendar.prototype.daysInWeek), DefineIntrinsic("Temporal.Calendar.prototype.daysInYear", Calendar.prototype.daysInYear), DefineIntrinsic("Temporal.Calendar.prototype.era", Calendar.prototype.era), DefineIntrinsic("Temporal.Calendar.prototype.eraYear", Calendar.prototype.eraYear), DefineIntrinsic("Temporal.Calendar.prototype.fields", Calendar.prototype.fields), DefineIntrinsic("Temporal.Calendar.prototype.inLeapYear", Calendar.prototype.inLeapYear), DefineIntrinsic("Temporal.Calendar.prototype.mergeFields", Calendar.prototype.mergeFields), DefineIntrinsic("Temporal.Calendar.prototype.month", Calendar.prototype.month), DefineIntrinsic("Temporal.Calendar.prototype.monthCode", Calendar.prototype.monthCode), DefineIntrinsic("Temporal.Calendar.prototype.monthDayFromFields", Calendar.prototype.monthDayFromFields), DefineIntrinsic("Temporal.Calendar.prototype.monthsInYear", Calendar.prototype.monthsInYear), DefineIntrinsic("Temporal.Calendar.prototype.weekOfYear", Calendar.prototype.weekOfYear), DefineIntrinsic("Temporal.Calendar.prototype.year", Calendar.prototype.year), DefineIntrinsic("Temporal.Calendar.prototype.yearMonthFromFields", Calendar.prototype.yearMonthFromFields), DefineIntrinsic("Temporal.Calendar.prototype.yearOfWeek", Calendar.prototype.yearOfWeek), Ot.iso8601 = {
  dateFromFields: function dateFromFields(e, t, r) {
    var _RegulateISODate2;
    var o = PrepareTemporalFields(e, ["day", "month", "monthCode", "year"], ["year", "day"]);
    var n = ToTemporalOverflow(t);
    o = resolveNonLunisolarMonth(o);
    var _o33 = o,
      a = _o33.year,
      i = _o33.month,
      s = _o33.day;
    return (_RegulateISODate2 = RegulateISODate(a, i, s, n), a = _RegulateISODate2.year, i = _RegulateISODate2.month, s = _RegulateISODate2.day), CreateTemporalDate(a, i, s, r);
  },
  yearMonthFromFields: function yearMonthFromFields(e, t, r) {
    var _RegulateISOYearMonth;
    var o = PrepareTemporalFields(e, ["month", "monthCode", "year"], ["year"]);
    var n = ToTemporalOverflow(t);
    o = resolveNonLunisolarMonth(o);
    var _o34 = o,
      a = _o34.year,
      i = _o34.month;
    return (_RegulateISOYearMonth = function RegulateISOYearMonth(e, t, r) {
      var o = e,
        n = t;
      switch (r) {
        case "reject":
          RejectISODate(o, n, 1);
          break;
        case "constrain":
          var _ConstrainISODate2 = ConstrainISODate(o, n);
          o = _ConstrainISODate2.year;
          n = _ConstrainISODate2.month;
      }
      return {
        year: o,
        month: n
      };
    }(a, i, n), a = _RegulateISOYearMonth.year, i = _RegulateISOYearMonth.month), CreateTemporalYearMonth(a, i, r, 1);
  },
  monthDayFromFields: function monthDayFromFields(e, t, r) {
    var _RegulateISODate3;
    var o = PrepareTemporalFields(e, ["day", "month", "monthCode", "year"], ["day"]);
    var n = ToTemporalOverflow(t);
    if (void 0 !== o.month && void 0 === o.year && void 0 === o.monthCode) throw new TypeError("either year or monthCode required with month");
    var a = void 0 === o.monthCode;
    o = resolveNonLunisolarMonth(o);
    var _o35 = o,
      i = _o35.month,
      s = _o35.day,
      l = _o35.year;
    return (_RegulateISODate3 = RegulateISODate(a ? l : 1972, i, s, n), i = _RegulateISODate3.month, s = _RegulateISODate3.day), CreateTemporalMonthDay(i, s, r, 1972);
  },
  fields: function fields(e) {
    return e;
  },
  fieldKeysToIgnore: function fieldKeysToIgnore(e) {
    var t = new Dt();
    for (var _r58 = 0; _r58 < e.length; _r58++) {
      var _o36 = e[_r58];
      Call(vt, t, [_o36]), "month" === _o36 ? Call(vt, t, ["monthCode"]) : "monthCode" === _o36 && Call(vt, t, ["month"]);
    }
    return _toConsumableArray(Call(Ct, t, []));
  },
  dateAdd: function dateAdd(e, t, r, o, n, a, d) {
    var _AddISODate;
    var m = GetSlot(e, i),
      c = GetSlot(e, s),
      h = GetSlot(e, l);
    return (_AddISODate = AddISODate(m, c, h, t, r, o, n, a), m = _AddISODate.year, c = _AddISODate.month, h = _AddISODate.day), CreateTemporalDate(m, c, h, d);
  },
  dateUntil: function dateUntil(e, t, r) {
    return DifferenceISODate(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l), GetSlot(t, i), GetSlot(t, s), GetSlot(t, l), r);
  },
  year: function year(e) {
    return GetSlot(e, i);
  },
  era: function era() {},
  eraYear: function eraYear() {},
  month: function month(e) {
    return GetSlot(e, s);
  },
  monthCode: function monthCode(e) {
    return buildMonthCode(GetSlot(e, s));
  },
  day: function day(e) {
    return GetSlot(e, l);
  },
  dayOfWeek: function dayOfWeek(e) {
    return DayOfWeek(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l));
  },
  dayOfYear: function dayOfYear(e) {
    return DayOfYear(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l));
  },
  weekOfYear: function weekOfYear(e) {
    return WeekOfYear(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l)).week;
  },
  yearOfWeek: function yearOfWeek(e) {
    return WeekOfYear(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l)).year;
  },
  daysInWeek: function daysInWeek() {
    return 7;
  },
  daysInMonth: function daysInMonth(e) {
    return ISODaysInMonth(GetSlot(e, i), GetSlot(e, s));
  },
  daysInYear: function daysInYear(e) {
    var t = e;
    return HasSlot(t, i) || (t = ToTemporalDate(t)), LeapYear(GetSlot(t, i)) ? 366 : 365;
  },
  monthsInYear: function monthsInYear() {
    return 12;
  },
  inLeapYear: function inLeapYear(e) {
    var t = e;
    return HasSlot(t, i) || (t = ToTemporalDate(t)), LeapYear(GetSlot(t, i));
  }
};
var OneObjectCache = /*#__PURE__*/function () {
  function OneObjectCache(e) {
    _classCallCheck(this, OneObjectCache);
    if (this.map = new Map(), this.calls = 0, this.hits = 0, this.misses = 0, this.now = globalThis.performance ? globalThis.performance.now() : Date.now(), void 0 !== e) {
      var _t43 = 0;
      var _iterator17 = _createForOfIteratorHelper(e.map.entries()),
        _step17;
      try {
        for (_iterator17.s(); !(_step17 = _iterator17.n()).done;) {
          var _this$map;
          var _r59 = _step17.value;
          if (++_t43 > OneObjectCache.MAX_CACHE_ENTRIES) break;
          (_this$map = this.map).set.apply(_this$map, _toConsumableArray(_r59));
        }
      } catch (err) {
        _iterator17.e(err);
      } finally {
        _iterator17.f();
      }
    }
  }
  _createClass(OneObjectCache, [{
    key: "get",
    value: function get(e) {
      var t = this.map.get(e);
      return t && (this.hits++, this.report()), this.calls++, t;
    }
  }, {
    key: "set",
    value: function set(e, t) {
      this.map.set(e, t), this.misses++, this.report();
    }
  }, {
    key: "report",
    value: function report() {}
  }, {
    key: "setObject",
    value: function setObject(e) {
      if (OneObjectCache.objectMap.get(e)) throw new RangeError("object already cached");
      OneObjectCache.objectMap.set(e, this), this.report();
    }
  }], [{
    key: "getCacheForObject",
    value: function getCacheForObject(e) {
      var t = OneObjectCache.objectMap.get(e);
      return t || (t = new OneObjectCache(), OneObjectCache.objectMap.set(e, t)), t;
    }
  }]);
  return OneObjectCache;
}();
function toUtcIsoDateString(_ref8) {
  var e = _ref8.isoYear,
    t = _ref8.isoMonth,
    r = _ref8.isoDay;
  return "".concat(ISOYearString(e), "-").concat(ISODateTimePartString(t), "-").concat(ISODateTimePartString(r), "T00:00Z");
}
function simpleDateDiff(e, t) {
  return {
    years: e.year - t.year,
    months: e.month - t.month,
    days: e.day - t.day
  };
}
OneObjectCache.objectMap = new WeakMap(), OneObjectCache.MAX_CACHE_ENTRIES = 1e3;
var HelperBase = /*#__PURE__*/function () {
  function HelperBase() {
    _classCallCheck(this, HelperBase);
    this.eraLength = "short", this.hasEra = !0, this.erasBeginMidYear = !1;
  }
  _createClass(HelperBase, [{
    key: "getFormatter",
    value: function getFormatter() {
      return void 0 === this.formatter && (this.formatter = new ft("en-US-u-ca-".concat(this.id), {
        day: "numeric",
        month: "numeric",
        year: "numeric",
        era: this.eraLength,
        timeZone: "UTC"
      })), this.formatter;
    }
  }, {
    key: "isoToCalendarDate",
    value: function isoToCalendarDate(e, t) {
      var _this = this;
      var r = e.year,
        o = e.month,
        n = e.day,
        a = JSON.stringify({
          func: "isoToCalendarDate",
          isoYear: r,
          isoMonth: o,
          isoDay: n,
          id: this.id
        }),
        i = t.get(a);
      if (i) return i;
      var s = this.getFormatter();
      var l, d;
      try {
        d = toUtcIsoDateString({
          isoYear: r,
          isoMonth: o,
          isoDay: n
        }), l = s.formatToParts(new Date(d));
      } catch (e) {
        throw new RangeError("Invalid ISO date: ".concat(JSON.stringify({
          isoYear: r,
          isoMonth: o,
          isoDay: n
        })));
      }
      var m = {};
      var _iterator18 = _createForOfIteratorHelper(l),
        _step18;
      try {
        for (_iterator18.s(); !(_step18 = _iterator18.n()).done;) {
          var _step18$value = _step18.value,
            _e37 = _step18$value.type,
            _t45 = _step18$value.value;
          if ("year" === _e37 && (m.eraYear = +_t45), "relatedYear" === _e37 && (m.eraYear = +_t45), "month" === _e37) {
            var _e38 = /^([0-9]*)(.*?)$/.exec(_t45);
            if (!_e38 || 3 != _e38.length || !_e38[1] && !_e38[2]) throw new RangeError("Unexpected month: ".concat(_t45));
            if (m.month = _e38[1] ? +_e38[1] : 1, m.month < 1) throw new RangeError("Invalid month ".concat(_t45, " from ").concat(d, "[u-ca-").concat(this.id, "] (probably due to https://bugs.chromium.org/p/v8/issues/detail?id=10527)"));
            if (m.month > 13) throw new RangeError("Invalid month ".concat(_t45, " from ").concat(d, "[u-ca-").concat(this.id, "] (probably due to https://bugs.chromium.org/p/v8/issues/detail?id=10529)"));
            _e38[2] && (m.monthExtra = _e38[2]);
          }
          "day" === _e37 && (m.day = +_t45), this.hasEra && "era" === _e37 && null != _t45 && "" !== _t45 && (_t45 = _t45.split(" (")[0], m.era = _t45.normalize("NFD").replace(/(?:[\0-\x1F!-,\.\/:-@\[-`\{-\xA9\xAB-\xB4\xB6-\xB9\xBB-\xBF\xD7\xF7\u02C2-\u02C5\u02D2-\u02DF\u02E5-\u02EB\u02ED\u02EF-\u036F\u0375\u0378\u0379\u037E\u0380-\u0385\u0387\u038B\u038D\u03A2\u03F6\u0482-\u0489\u0530\u0557\u0558\u055A-\u055F\u0589-\u05CF\u05EB-\u05EE\u05F3-\u061F\u064B-\u066D\u0670\u06D4\u06D6-\u06E4\u06E7-\u06ED\u06F0-\u06F9\u06FD\u06FE\u0700-\u070F\u0711\u0730-\u074C\u07A6-\u07B0\u07B2-\u07C9\u07EB-\u07F3\u07F6-\u07F9\u07FB-\u07FF\u0816-\u0819\u081B-\u0823\u0825-\u0827\u0829-\u083F\u0859-\u085F\u086B-\u086F\u0888\u088F-\u089F\u08CA-\u0903\u093A-\u093C\u093E-\u094F\u0951-\u0957\u0962-\u0970\u0981-\u0984\u098D\u098E\u0991\u0992\u09A9\u09B1\u09B3-\u09B5\u09BA-\u09BC\u09BE-\u09CD\u09CF-\u09DB\u09DE\u09E2-\u09EF\u09F2-\u09FB\u09FD-\u0A04\u0A0B-\u0A0E\u0A11\u0A12\u0A29\u0A31\u0A34\u0A37\u0A3A-\u0A58\u0A5D\u0A5F-\u0A71\u0A75-\u0A84\u0A8E\u0A92\u0AA9\u0AB1\u0AB4\u0ABA-\u0ABC\u0ABE-\u0ACF\u0AD1-\u0ADF\u0AE2-\u0AF8\u0AFA-\u0B04\u0B0D\u0B0E\u0B11\u0B12\u0B29\u0B31\u0B34\u0B3A-\u0B3C\u0B3E-\u0B5B\u0B5E\u0B62-\u0B70\u0B72-\u0B82\u0B84\u0B8B-\u0B8D\u0B91\u0B96-\u0B98\u0B9B\u0B9D\u0BA0-\u0BA2\u0BA5-\u0BA7\u0BAB-\u0BAD\u0BBA-\u0BCF\u0BD1-\u0C04\u0C0D\u0C11\u0C29\u0C3A-\u0C3C\u0C3E-\u0C57\u0C5B\u0C5C\u0C5E\u0C5F\u0C62-\u0C7F\u0C81-\u0C84\u0C8D\u0C91\u0CA9\u0CB4\u0CBA-\u0CBC\u0CBE-\u0CDC\u0CDF\u0CE2-\u0CF0\u0CF3-\u0D03\u0D0D\u0D11\u0D3B\u0D3C\u0D3E-\u0D4D\u0D4F-\u0D53\u0D57-\u0D5E\u0D62-\u0D79\u0D80-\u0D84\u0D97-\u0D99\u0DB2\u0DBC\u0DBE\u0DBF\u0DC7-\u0E00\u0E31\u0E34-\u0E3F\u0E47-\u0E80\u0E83\u0E85\u0E8B\u0EA4\u0EA6\u0EB1\u0EB4-\u0EBC\u0EBE\u0EBF\u0EC5\u0EC7-\u0EDB\u0EE0-\u0EFF\u0F01-\u0F3F\u0F48\u0F6D-\u0F87\u0F8D-\u0FFF\u102B-\u103E\u1040-\u104F\u1056-\u1059\u105E-\u1060\u1062-\u1064\u1067-\u106D\u1071-\u1074\u1082-\u108D\u108F-\u109F\u10C6\u10C8-\u10CC\u10CE\u10CF\u10FB\u1249\u124E\u124F\u1257\u1259\u125E\u125F\u1289\u128E\u128F\u12B1\u12B6\u12B7\u12BF\u12C1\u12C6\u12C7\u12D7\u1311\u1316\u1317\u135B-\u137F\u1390-\u139F\u13F6\u13F7\u13FE-\u1400\u166D\u166E\u1680\u169B-\u169F\u16EB-\u16F0\u16F9-\u16FF\u1712-\u171E\u1732-\u173F\u1752-\u175F\u176D\u1771-\u177F\u17B4-\u17D6\u17D8-\u17DB\u17DD-\u181F\u1879-\u187F\u1885\u1886\u18A9\u18AB-\u18AF\u18F6-\u18FF\u191F-\u194F\u196E\u196F\u1975-\u197F\u19AC-\u19AF\u19CA-\u19FF\u1A17-\u1A1F\u1A55-\u1AA6\u1AA8-\u1B04\u1B34-\u1B44\u1B4D-\u1B82\u1BA1-\u1BAD\u1BB0-\u1BB9\u1BE6-\u1BFF\u1C24-\u1C4C\u1C50-\u1C59\u1C7E\u1C7F\u1C89-\u1C8F\u1CBB\u1CBC\u1CC0-\u1CE8\u1CED\u1CF4\u1CF7-\u1CF9\u1CFB-\u1CFF\u1DC0-\u1DFF\u1F16\u1F17\u1F1E\u1F1F\u1F46\u1F47\u1F4E\u1F4F\u1F58\u1F5A\u1F5C\u1F5E\u1F7E\u1F7F\u1FB5\u1FBD\u1FBF-\u1FC1\u1FC5\u1FCD-\u1FCF\u1FD4\u1FD5\u1FDC-\u1FDF\u1FED-\u1FF1\u1FF5\u1FFD-\u2070\u2072-\u207E\u2080-\u208F\u209D-\u2101\u2103-\u2106\u2108\u2109\u2114\u2116-\u2118\u211E-\u2123\u2125\u2127\u2129\u212E\u213A\u213B\u2140-\u2144\u214A-\u214D\u214F-\u2182\u2185-\u2BFF\u2CE5-\u2CEA\u2CEF-\u2CF1\u2CF4-\u2CFF\u2D26\u2D28-\u2D2C\u2D2E\u2D2F\u2D68-\u2D6E\u2D70-\u2D7F\u2D97-\u2D9F\u2DA7\u2DAF\u2DB7\u2DBF\u2DC7\u2DCF\u2DD7\u2DDF-\u2E2E\u2E30-\u3004\u3007-\u3030\u3036-\u303A\u303D-\u3040\u3097-\u309C\u30A0\u30FB\u3100-\u3104\u3130\u318F-\u319F\u31C0-\u31EF\u3200-\u33FF\u4DC0-\u4DFF\uA48D-\uA4CF\uA4FE\uA4FF\uA60D-\uA60F\uA620-\uA629\uA62C-\uA63F\uA66F-\uA67E\uA69E\uA69F\uA6E6-\uA716\uA720\uA721\uA789\uA78A\uA7CB-\uA7CF\uA7D2\uA7D4\uA7DA-\uA7F1\uA802\uA806\uA80B\uA823-\uA83F\uA874-\uA881\uA8B4-\uA8F1\uA8F8-\uA8FA\uA8FC\uA8FF-\uA909\uA926-\uA92F\uA947-\uA95F\uA97D-\uA983\uA9B3-\uA9CE\uA9D0-\uA9DF\uA9E5\uA9F0-\uA9F9\uA9FF\uAA29-\uAA3F\uAA43\uAA4C-\uAA5F\uAA77-\uAA79\uAA7B-\uAA7D\uAAB0\uAAB2-\uAAB4\uAAB7\uAAB8\uAABE\uAABF\uAAC1\uAAC3-\uAADA\uAADE\uAADF\uAAEB-\uAAF1\uAAF5-\uAB00\uAB07\uAB08\uAB0F\uAB10\uAB17-\uAB1F\uAB27\uAB2F\uAB5B\uAB6A-\uAB6F\uABE3-\uABFF\uD7A4-\uD7AF\uD7C7-\uD7CA\uD7FC-\uD7FF\uE000-\uF8FF\uFA6E\uFA6F\uFADA-\uFAFF\uFB07-\uFB12\uFB18-\uFB1C\uFB1E\uFB29\uFB37\uFB3D\uFB3F\uFB42\uFB45\uFBB2-\uFBD2\uFD3E-\uFD4F\uFD90\uFD91\uFDC8-\uFDEF\uFDFC-\uFE6F\uFE75\uFEFD-\uFF20\uFF3B-\uFF40\uFF5B-\uFF65\uFFBF-\uFFC1\uFFC8\uFFC9\uFFD0\uFFD1\uFFD8\uFFD9\uFFDD-\uFFFF]|\uD800[\uDC0C\uDC27\uDC3B\uDC3E\uDC4E\uDC4F\uDC5E-\uDC7F\uDCFB-\uDE7F\uDE9D-\uDE9F\uDED1-\uDEFF\uDF20-\uDF2C\uDF41\uDF4A-\uDF4F\uDF76-\uDF7F\uDF9E\uDF9F\uDFC4-\uDFC7\uDFD0-\uDFFF]|\uD801[\uDC9E-\uDCAF\uDCD4-\uDCD7\uDCFC-\uDCFF\uDD28-\uDD2F\uDD64-\uDD6F\uDD7B\uDD8B\uDD93\uDD96\uDDA2\uDDB2\uDDBA\uDDBD-\uDDFF\uDF37-\uDF3F\uDF56-\uDF5F\uDF68-\uDF7F\uDF86\uDFB1\uDFBB-\uDFFF]|\uD802[\uDC06\uDC07\uDC09\uDC36\uDC39-\uDC3B\uDC3D\uDC3E\uDC56-\uDC5F\uDC77-\uDC7F\uDC9F-\uDCDF\uDCF3\uDCF6-\uDCFF\uDD16-\uDD1F\uDD3A-\uDD7F\uDDB8-\uDDBD\uDDC0-\uDDFF\uDE01-\uDE0F\uDE14\uDE18\uDE36-\uDE5F\uDE7D-\uDE7F\uDE9D-\uDEBF\uDEC8\uDEE5-\uDEFF\uDF36-\uDF3F\uDF56-\uDF5F\uDF73-\uDF7F\uDF92-\uDFFF]|\uD803[\uDC49-\uDC7F\uDCB3-\uDCBF\uDCF3-\uDCFF\uDD24-\uDE7F\uDEAA-\uDEAF\uDEB2-\uDEFF\uDF1D-\uDF26\uDF28-\uDF2F\uDF46-\uDF6F\uDF82-\uDFAF\uDFC5-\uDFDF\uDFF7-\uDFFF]|\uD804[\uDC00-\uDC02\uDC38-\uDC70\uDC73\uDC74\uDC76-\uDC82\uDCB0-\uDCCF\uDCE9-\uDD02\uDD27-\uDD43\uDD45\uDD46\uDD48-\uDD4F\uDD73-\uDD75\uDD77-\uDD82\uDDB3-\uDDC0\uDDC5-\uDDD9\uDDDB\uDDDD-\uDDFF\uDE12\uDE2C-\uDE3E\uDE41-\uDE7F\uDE87\uDE89\uDE8E\uDE9E\uDEA9-\uDEAF\uDEDF-\uDF04\uDF0D\uDF0E\uDF11\uDF12\uDF29\uDF31\uDF34\uDF3A-\uDF3C\uDF3E-\uDF4F\uDF51-\uDF5C\uDF62-\uDFFF]|\uD805[\uDC35-\uDC46\uDC4B-\uDC5E\uDC62-\uDC7F\uDCB0-\uDCC3\uDCC6\uDCC8-\uDD7F\uDDAF-\uDDD7\uDDDC-\uDDFF\uDE30-\uDE43\uDE45-\uDE7F\uDEAB-\uDEB7\uDEB9-\uDEFF\uDF1B-\uDF3F\uDF47-\uDFFF]|\uD806[\uDC2C-\uDC9F\uDCE0-\uDCFE\uDD07\uDD08\uDD0A\uDD0B\uDD14\uDD17\uDD30-\uDD3E\uDD40\uDD42-\uDD9F\uDDA8\uDDA9\uDDD1-\uDDE0\uDDE2\uDDE4-\uDDFF\uDE01-\uDE0A\uDE33-\uDE39\uDE3B-\uDE4F\uDE51-\uDE5B\uDE8A-\uDE9C\uDE9E-\uDEAF\uDEF9-\uDFFF]|\uD807[\uDC09\uDC2F-\uDC3F\uDC41-\uDC71\uDC90-\uDCFF\uDD07\uDD0A\uDD31-\uDD45\uDD47-\uDD5F\uDD66\uDD69\uDD8A-\uDD97\uDD99-\uDEDF\uDEF3-\uDF01\uDF03\uDF11\uDF34-\uDFAF\uDFB1-\uDFFF]|\uD808[\uDF9A-\uDFFF]|\uD809[\uDC00-\uDC7F\uDD44-\uDFFF]|[\uD80A\uD80E-\uD810\uD812-\uD819\uD824-\uD82A\uD82D\uD82E\uD830-\uD834\uD836\uD83C-\uD83F\uD87C\uD87D\uD87F\uD889-\uDBFF][\uDC00-\uDFFF]|\uD80B[\uDC00-\uDF8F\uDFF1-\uDFFF]|\uD80D[\uDC30-\uDC40\uDC47-\uDFFF]|\uD811[\uDE47-\uDFFF]|\uD81A[\uDE39-\uDE3F\uDE5F-\uDE6F\uDEBF-\uDECF\uDEEE-\uDEFF\uDF30-\uDF3F\uDF44-\uDF62\uDF78-\uDF7C\uDF90-\uDFFF]|\uD81B[\uDC00-\uDE3F\uDE80-\uDEFF\uDF4B-\uDF4F\uDF51-\uDF92\uDFA0-\uDFDF\uDFE2\uDFE4-\uDFFF]|\uD821[\uDFF8-\uDFFF]|\uD823[\uDCD6-\uDCFF\uDD09-\uDFFF]|\uD82B[\uDC00-\uDFEF\uDFF4\uDFFC\uDFFF]|\uD82C[\uDD23-\uDD31\uDD33-\uDD4F\uDD53\uDD54\uDD56-\uDD63\uDD68-\uDD6F\uDEFC-\uDFFF]|\uD82F[\uDC6B-\uDC6F\uDC7D-\uDC7F\uDC89-\uDC8F\uDC9A-\uDFFF]|\uD835[\uDC55\uDC9D\uDCA0\uDCA1\uDCA3\uDCA4\uDCA7\uDCA8\uDCAD\uDCBA\uDCBC\uDCC4\uDD06\uDD0B\uDD0C\uDD15\uDD1D\uDD3A\uDD3F\uDD45\uDD47-\uDD49\uDD51\uDEA6\uDEA7\uDEC1\uDEDB\uDEFB\uDF15\uDF35\uDF4F\uDF6F\uDF89\uDFA9\uDFC3\uDFCC-\uDFFF]|\uD837[\uDC00-\uDEFF\uDF1F-\uDF24\uDF2B-\uDFFF]|\uD838[\uDC00-\uDC2F\uDC6E-\uDCFF\uDD2D-\uDD36\uDD3E-\uDD4D\uDD4F-\uDE8F\uDEAE-\uDEBF\uDEEC-\uDFFF]|\uD839[\uDC00-\uDCCF\uDCEC-\uDFDF\uDFE7\uDFEC\uDFEF\uDFFF]|\uD83A[\uDCC5-\uDCFF\uDD44-\uDD4A\uDD4C-\uDFFF]|\uD83B[\uDC00-\uDDFF\uDE04\uDE20\uDE23\uDE25\uDE26\uDE28\uDE33\uDE38\uDE3A\uDE3C-\uDE41\uDE43-\uDE46\uDE48\uDE4A\uDE4C\uDE50\uDE53\uDE55\uDE56\uDE58\uDE5A\uDE5C\uDE5E\uDE60\uDE63\uDE65\uDE66\uDE6B\uDE73\uDE78\uDE7D\uDE7F\uDE8A\uDE9C-\uDEA0\uDEA4\uDEAA\uDEBC-\uDFFF]|\uD869[\uDEE0-\uDEFF]|\uD86D[\uDF3A-\uDF3F]|\uD86E[\uDC1E\uDC1F]|\uD873[\uDEA2-\uDEAF]|\uD87A[\uDFE1-\uDFEF]|\uD87B[\uDE5E-\uDFFF]|\uD87E[\uDE1E-\uDFFF]|\uD884[\uDF4B-\uDF4F]|\uD888[\uDFB0-\uDFFF]|[\uD800-\uDBFF](?![\uDC00-\uDFFF])|(?:[^\uD800-\uDBFF]|^)[\uDC00-\uDFFF])/g, "").replace(" ", "-").toLowerCase());
        }
      } catch (err) {
        _iterator18.e(err);
      } finally {
        _iterator18.f();
      }
      if (void 0 === m.eraYear) throw new RangeError("Intl.DateTimeFormat.formatToParts lacks relatedYear in ".concat(this.id, " calendar. Try Node 14+ or modern browsers."));
      if (this.reviseIntlEra) {
        var _this$reviseIntlEra = this.reviseIntlEra(m, e),
          _t44 = _this$reviseIntlEra.era,
          _r60 = _this$reviseIntlEra.eraYear;
        m.era = _t44, m.eraYear = _r60;
      }
      this.checkIcuBugs && this.checkIcuBugs(e);
      var c = this.adjustCalendarDate(m, t, "constrain", !0);
      if (void 0 === c.year) throw new RangeError("Missing year converting ".concat(JSON.stringify(e)));
      if (void 0 === c.month) throw new RangeError("Missing month converting ".concat(JSON.stringify(e)));
      if (void 0 === c.day) throw new RangeError("Missing day converting ".concat(JSON.stringify(e)));
      return t.set(a, c), ["constrain", "reject"].forEach(function (r) {
        var o = JSON.stringify({
          func: "calendarToIsoDate",
          year: c.year,
          month: c.month,
          day: c.day,
          overflow: r,
          id: _this.id
        });
        t.set(o, e);
      }), c;
    }
  }, {
    key: "validateCalendarDate",
    value: function validateCalendarDate(e) {
      var t = e.era,
        r = e.month,
        o = e.year,
        n = e.day,
        a = e.eraYear,
        i = e.monthCode,
        s = e.monthExtra;
      if (void 0 !== s) throw new RangeError("Unexpected `monthExtra` value");
      if (void 0 === o && void 0 === a) throw new TypeError("year or eraYear is required");
      if (void 0 === r && void 0 === i) throw new TypeError("month or monthCode is required");
      if (void 0 === n) throw new RangeError("Missing day");
      if (void 0 !== i) {
        if ("string" != typeof i) throw new RangeError("monthCode must be a string, not " + _typeof(i));
        if (!/^M([01]?\d)(L?)$/.test(i)) throw new RangeError("Invalid monthCode: ".concat(i));
      }
      if (this.constantEra) {
        if (void 0 !== t && t !== this.constantEra) throw new RangeError("era must be ".concat(this.constantEra, ", not ").concat(t));
        if (void 0 !== a && void 0 !== o && a !== o) throw new RangeError("eraYear ".concat(a, " does not match year ").concat(o));
      }
      if (this.hasEra && void 0 === e.era != (void 0 === e.eraYear)) throw new RangeError("properties 'era' and 'eraYear' must be provided together");
    }
  }, {
    key: "adjustCalendarDate",
    value: function adjustCalendarDate(e, t) {
      var _resolveNonLunisolarM;
      var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "constrain";
      if ("lunisolar" === this.calendarType) throw new RangeError("Override required for lunisolar calendars");
      var n = e;
      if (this.validateCalendarDate(n), this.constantEra) {
        var _n21 = n,
          _e39 = _n21.year,
          _t46 = _n21.eraYear;
        n = _objectSpread2(_objectSpread2({}, n), {}, {
          era: this.constantEra,
          year: void 0 !== _e39 ? _e39 : _t46,
          eraYear: void 0 !== _t46 ? _t46 : _e39
        });
      }
      var a = this.monthsInYear(n, t);
      var _n22 = n,
        i = _n22.month,
        s = _n22.monthCode;
      return (_resolveNonLunisolarM = resolveNonLunisolarMonth(n, r, a), i = _resolveNonLunisolarM.month, s = _resolveNonLunisolarM.monthCode), _objectSpread2(_objectSpread2({}, n), {}, {
        month: i,
        monthCode: s
      });
    }
  }, {
    key: "regulateMonthDayNaive",
    value: function regulateMonthDayNaive(e, t, r) {
      var o = this.monthsInYear(e, r);
      var n = e.month,
        a = e.day;
      return "reject" === t ? (RejectToRange(n, 1, o), RejectToRange(a, 1, this.maximumMonthLength(e))) : (n = ConstrainToRange(n, 1, o), a = ConstrainToRange(a, 1, this.maximumMonthLength(_objectSpread2(_objectSpread2({}, e), {}, {
        month: n
      })))), _objectSpread2(_objectSpread2({}, e), {}, {
        month: n,
        day: a
      });
    }
  }, {
    key: "calendarToIsoDate",
    value: function calendarToIsoDate(e) {
      var _this2 = this;
      var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "constrain";
      var r = arguments.length > 2 ? arguments[2] : undefined;
      var o = e;
      var n = this.adjustCalendarDate(e, r, t, !1);
      n = this.regulateMonthDayNaive(n, t, r);
      var _n23 = n,
        a = _n23.year,
        i = _n23.month,
        s = _n23.day,
        l = JSON.stringify({
          func: "calendarToIsoDate",
          year: a,
          month: i,
          day: s,
          overflow: t,
          id: this.id
        });
      var d,
        m = r.get(l);
      if (m) return m;
      if (void 0 !== o.year && void 0 !== o.month && void 0 !== o.day && (o.year !== n.year || o.month !== n.month || o.day !== n.day) && (d = JSON.stringify({
        func: "calendarToIsoDate",
        year: o.year,
        month: o.month,
        day: o.day,
        overflow: t,
        id: this.id
      }), m = r.get(d), m)) return m;
      var c = this.estimateIsoDate({
        year: a,
        month: i,
        day: s
      });
      var calculateSameMonthResult = function calculateSameMonthResult(e) {
        var o = _this2.addDaysIso(c, e);
        if (n.day > _this2.minimumMonthLength(n)) {
          var _e40 = _this2.isoToCalendarDate(o, r);
          for (; _e40.month !== i || _e40.year !== a;) {
            if ("reject" === t) throw new RangeError("day ".concat(s, " does not exist in month ").concat(i, " of year ").concat(a));
            o = _this2.addDaysIso(o, -1), _e40 = _this2.isoToCalendarDate(o, r);
          }
        }
        return o;
      };
      var h = 0,
        u = this.isoToCalendarDate(c, r),
        T = simpleDateDiff(n, u);
      if (0 !== T.years || 0 !== T.months || 0 !== T.days) {
        var _e41 = 365 * T.years + 30 * T.months + T.days;
        c = this.addDaysIso(c, _e41), u = this.isoToCalendarDate(c, r), T = simpleDateDiff(n, u), 0 === T.years && 0 === T.months ? c = calculateSameMonthResult(T.days) : h = this.compareCalendarDates(n, u);
      }
      var p = 8;
      for (; h;) {
        c = this.addDaysIso(c, h * p);
        var _e42 = u;
        u = this.isoToCalendarDate(c, r);
        var _a17 = h;
        if (h = this.compareCalendarDates(n, u), h) if (T = simpleDateDiff(n, u), 0 === T.years && 0 === T.months) c = calculateSameMonthResult(T.days), h = 0;else if (_a17 && h !== _a17) if (p > 1) p /= 2;else {
          if ("reject" === t) throw new RangeError("Can't find ISO date from calendar date: ".concat(JSON.stringify(_objectSpread2({}, o))));
          this.compareCalendarDates(u, _e42) > 0 && (c = this.addDaysIso(c, -1)), h = 0;
        }
      }
      if (r.set(l, c), d && r.set(d, c), void 0 === n.year || void 0 === n.month || void 0 === n.day || void 0 === n.monthCode || this.hasEra && (void 0 === n.era || void 0 === n.eraYear)) throw new RangeError("Unexpected missing property");
      return c;
    }
  }, {
    key: "temporalToCalendarDate",
    value: function temporalToCalendarDate(e, t) {
      var r = {
        year: GetSlot(e, i),
        month: GetSlot(e, s),
        day: GetSlot(e, l)
      };
      return this.isoToCalendarDate(r, t);
    }
  }, {
    key: "compareCalendarDates",
    value: function compareCalendarDates(e, t) {
      var r = PrepareTemporalFields(e, ["day", "month", "year"], ["day", "month", "year"]),
        o = PrepareTemporalFields(t, ["day", "month", "year"], ["day", "month", "year"]);
      return r.year !== o.year ? ComparisonResult(r.year - o.year) : r.month !== o.month ? ComparisonResult(r.month - o.month) : r.day !== o.day ? ComparisonResult(r.day - o.day) : 0;
    }
  }, {
    key: "regulateDate",
    value: function regulateDate(e) {
      var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "constrain";
      var r = arguments.length > 2 ? arguments[2] : undefined;
      var o = this.calendarToIsoDate(e, t, r);
      return this.isoToCalendarDate(o, r);
    }
  }, {
    key: "addDaysIso",
    value: function addDaysIso(e, t) {
      return AddISODate(e.year, e.month, e.day, 0, 0, 0, t, "constrain");
    }
  }, {
    key: "addDaysCalendar",
    value: function addDaysCalendar(e, t, r) {
      var o = this.calendarToIsoDate(e, "constrain", r),
        n = this.addDaysIso(o, t);
      return this.isoToCalendarDate(n, r);
    }
  }, {
    key: "addMonthsCalendar",
    value: function addMonthsCalendar(e, t, r, o) {
      var n = e;
      var _n24 = n,
        a = _n24.day;
      for (var _e43 = 0, _r61 = It(t); _e43 < _r61; _e43++) {
        var _n25 = n,
          _e44 = _n25.month,
          _r62 = n,
          _i18 = t < 0 ? -Math.max(a, this.daysInPreviousMonth(n, o)) : this.daysInMonth(n, o),
          _s7 = this.calendarToIsoDate(n, "constrain", o);
        var _l7 = this.addDaysIso(_s7, _i18);
        if (n = this.isoToCalendarDate(_l7, o), t > 0) {
          var _t47 = this.monthsInYear(_r62, o);
          for (; n.month - 1 != _e44 % _t47;) _l7 = this.addDaysIso(_l7, -1), n = this.isoToCalendarDate(_l7, o);
        }
        n.day !== a && (n = this.regulateDate(_objectSpread2(_objectSpread2({}, n), {}, {
          day: a
        }), "constrain", o));
      }
      if ("reject" === r && n.day !== a) throw new RangeError("Day ".concat(a, " does not exist in resulting calendar month"));
      return n;
    }
  }, {
    key: "addCalendar",
    value: function addCalendar(e, _ref9, a, i) {
      var _ref9$years = _ref9.years,
        t = _ref9$years === void 0 ? 0 : _ref9$years,
        _ref9$months = _ref9.months,
        r = _ref9$months === void 0 ? 0 : _ref9$months,
        _ref9$weeks = _ref9.weeks,
        o = _ref9$weeks === void 0 ? 0 : _ref9$weeks,
        _ref9$days = _ref9.days,
        n = _ref9$days === void 0 ? 0 : _ref9$days;
      var s = e.year,
        l = e.day,
        d = e.monthCode,
        m = this.adjustCalendarDate({
          year: s + t,
          monthCode: d,
          day: l
        }, i),
        c = this.addMonthsCalendar(m, r, a, i),
        h = n + 7 * o;
      return this.addDaysCalendar(c, h, i);
    }
  }, {
    key: "untilCalendar",
    value: function untilCalendar(e, t, r, o) {
      var n = 0,
        a = 0,
        i = 0,
        s = 0;
      switch (r) {
        case "day":
          n = this.calendarDaysUntil(e, t, o);
          break;
        case "week":
          {
            var _r63 = this.calendarDaysUntil(e, t, o);
            n = _r63 % 7, a = (_r63 - n) / 7;
            break;
          }
        case "month":
        case "year":
          {
            var _a18 = this.compareCalendarDates(t, e);
            if (!_a18) return {
              years: 0,
              months: 0,
              weeks: 0,
              days: 0
            };
            var _l8 = t.year - e.year,
              _d10 = t.day - e.day;
            if ("year" === r && _l8) {
              var _r64 = 0;
              t.monthCode > e.monthCode && (_r64 = 1), t.monthCode < e.monthCode && (_r64 = -1), _r64 || (_r64 = Math.sign(_d10));
              s = _r64 * _a18 < 0 ? _l8 - _a18 : _l8;
            }
            var _m9,
              _c5 = s ? this.addCalendar(e, {
                years: s
              }, "constrain", o) : e;
            do {
              i += _a18, _m9 = _c5, _c5 = this.addMonthsCalendar(_m9, _a18, "constrain", o), _c5.day !== e.day && (_c5 = this.regulateDate(_objectSpread2(_objectSpread2({}, _c5), {}, {
                day: e.day
              }), "constrain", o));
            } while (this.compareCalendarDates(t, _c5) * _a18 >= 0);
            i -= _a18;
            n = this.calendarDaysUntil(_m9, t, o);
            break;
          }
      }
      return {
        years: s,
        months: i,
        weeks: a,
        days: n
      };
    }
  }, {
    key: "daysInMonth",
    value: function daysInMonth(e, t) {
      var r = e.day,
        o = this.maximumMonthLength(e),
        n = this.minimumMonthLength(e);
      if (n === o) return n;
      var a = r <= o - n ? o : n,
        i = this.calendarToIsoDate(e, "constrain", t),
        s = this.addDaysIso(i, a),
        l = this.isoToCalendarDate(s, t),
        d = this.addDaysIso(s, -l.day);
      return this.isoToCalendarDate(d, t).day;
    }
  }, {
    key: "daysInPreviousMonth",
    value: function daysInPreviousMonth(e, t) {
      var r = e.day,
        o = e.month,
        n = e.year;
      var a = {
        year: o > 1 ? n : n - 1,
        month: o,
        day: 1
      };
      var i = o > 1 ? o - 1 : this.monthsInYear(a, t);
      a = _objectSpread2(_objectSpread2({}, a), {}, {
        month: i
      });
      var s = this.minimumMonthLength(a),
        l = this.maximumMonthLength(a);
      if (s === l) return l;
      var d = this.calendarToIsoDate(e, "constrain", t),
        m = this.addDaysIso(d, -r);
      return this.isoToCalendarDate(m, t).day;
    }
  }, {
    key: "startOfCalendarYear",
    value: function startOfCalendarYear(e) {
      return {
        year: e.year,
        month: 1,
        monthCode: "M01",
        day: 1
      };
    }
  }, {
    key: "startOfCalendarMonth",
    value: function startOfCalendarMonth(e) {
      return {
        year: e.year,
        month: e.month,
        day: 1
      };
    }
  }, {
    key: "calendarDaysUntil",
    value: function calendarDaysUntil(e, t, r) {
      var o = this.calendarToIsoDate(e, "constrain", r),
        n = this.calendarToIsoDate(t, "constrain", r);
      return this.isoDaysUntil(o, n);
    }
  }, {
    key: "isoDaysUntil",
    value: function isoDaysUntil(e, t) {
      return DifferenceISODate(e.year, e.month, e.day, t.year, t.month, t.day, "day").days;
    }
  }, {
    key: "monthDayFromFields",
    value: function monthDayFromFields(e, t, r) {
      var o,
        n,
        a,
        i,
        s,
        l = e.monthCode,
        d = e.day;
      if (void 0 === l) {
        var _o37 = e.year,
          _n26 = e.era,
          _a19 = e.eraYear;
        if (void 0 === _o37 && (void 0 === _n26 || void 0 === _a19)) throw new TypeError("when `monthCode` is omitted, `year` (or `era` and `eraYear`) and `month` are required");
        var _this$isoToCalendarDa = this.isoToCalendarDate(this.calendarToIsoDate(e, t, r), r);
        l = _this$isoToCalendarDa.monthCode;
        d = _this$isoToCalendarDa.day;
      }
      var m = this.isoToCalendarDate({
          year: 1972,
          month: 12,
          day: 31
        }, r),
        c = m.monthCode > l || m.monthCode === l && m.day >= d ? m.year : m.year - 1;
      for (var _e45 = 0; _e45 < 100; _e45++) {
        var _m10 = this.adjustCalendarDate({
            day: d,
            monthCode: l,
            year: c - _e45
          }, r),
          _h3 = this.calendarToIsoDate(_m10, "constrain", r),
          _u2 = this.isoToCalendarDate(_h3, r);
        if ((o = _h3.year, n = _h3.month, a = _h3.day), _u2.monthCode === l && _u2.day === d) return {
          month: n,
          day: a,
          year: o
        };
        "constrain" === t && (void 0 === i || _u2.monthCode === i.monthCode && _u2.day > i.day) && (i = _u2, s = _h3);
      }
      if ("constrain" === t && void 0 !== s) return s;
      throw new RangeError("No recent ".concat(this.id, " year with monthCode ").concat(l, " and day ").concat(d));
    }
  }]);
  return HelperBase;
}();
var HebrewHelper = /*#__PURE__*/function (_HelperBase) {
  _inherits(HebrewHelper, _HelperBase);
  var _super = _createSuper(HebrewHelper);
  function HebrewHelper() {
    var _this3;
    _classCallCheck(this, HebrewHelper);
    _this3 = _super.apply(this, arguments), _this3.id = "hebrew", _this3.calendarType = "lunisolar", _this3.months = {
      Tishri: {
        leap: 1,
        regular: 1,
        monthCode: "M01",
        days: 30
      },
      Heshvan: {
        leap: 2,
        regular: 2,
        monthCode: "M02",
        days: {
          min: 29,
          max: 30
        }
      },
      Kislev: {
        leap: 3,
        regular: 3,
        monthCode: "M03",
        days: {
          min: 29,
          max: 30
        }
      },
      Tevet: {
        leap: 4,
        regular: 4,
        monthCode: "M04",
        days: 29
      },
      Shevat: {
        leap: 5,
        regular: 5,
        monthCode: "M05",
        days: 30
      },
      Adar: {
        leap: void 0,
        regular: 6,
        monthCode: "M06",
        days: 29
      },
      "Adar I": {
        leap: 6,
        regular: void 0,
        monthCode: "M05L",
        days: 30
      },
      "Adar II": {
        leap: 7,
        regular: void 0,
        monthCode: "M06",
        days: 29
      },
      Nisan: {
        leap: 8,
        regular: 7,
        monthCode: "M07",
        days: 30
      },
      Iyar: {
        leap: 9,
        regular: 8,
        monthCode: "M08",
        days: 29
      },
      Sivan: {
        leap: 10,
        regular: 9,
        monthCode: "M09",
        days: 30
      },
      Tamuz: {
        leap: 11,
        regular: 10,
        monthCode: "M10",
        days: 29
      },
      Av: {
        leap: 12,
        regular: 11,
        monthCode: "M11",
        days: 30
      },
      Elul: {
        leap: 13,
        regular: 12,
        monthCode: "M12",
        days: 29
      }
    }, _this3.hasEra = !1;
    return _this3;
  }
  _createClass(HebrewHelper, [{
    key: "inLeapYear",
    value: function inLeapYear(e) {
      var t = e.year;
      return (7 * t + 1) % 19 < 7;
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear(e) {
      return this.inLeapYear(e) ? 13 : 12;
    }
  }, {
    key: "minimumMonthLength",
    value: function minimumMonthLength(e) {
      return this.minMaxMonthLength(e, "min");
    }
  }, {
    key: "maximumMonthLength",
    value: function maximumMonthLength(e) {
      return this.minMaxMonthLength(e, "max");
    }
  }, {
    key: "minMaxMonthLength",
    value: function minMaxMonthLength(e, t) {
      var r = e.month,
        o = e.year,
        n = this.getMonthCode(o, r),
        a = wt(this.months).find(function (e) {
          return e[1].monthCode === n;
        });
      if (void 0 === a) throw new RangeError("unmatched Hebrew month: ".concat(r));
      var i = a[1].days;
      return "number" == typeof i ? i : i[t];
    }
  }, {
    key: "estimateIsoDate",
    value: function estimateIsoDate(e) {
      var t = e.year;
      return {
        year: t - 3760,
        month: 1,
        day: 1
      };
    }
  }, {
    key: "getMonthCode",
    value: function getMonthCode(e, t) {
      return this.inLeapYear({
        year: e
      }) ? 6 === t ? buildMonthCode(5, !0) : buildMonthCode(t < 6 ? t : t - 1) : buildMonthCode(t);
    }
  }, {
    key: "adjustCalendarDate",
    value: function adjustCalendarDate(e, t) {
      var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "constrain";
      var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : !1;
      var n = e.year,
        a = e.eraYear,
        i = e.month,
        s = e.monthCode,
        l = e.day,
        d = e.monthExtra;
      if (void 0 === n && void 0 !== a && (n = a), void 0 === a && void 0 !== n && (a = n), o) {
        if (d) {
          var _e46 = this.months[d];
          if (!_e46) throw new RangeError("Unrecognized month from formatToParts: ".concat(d));
          i = this.inLeapYear({
            year: n
          }) ? _e46.leap : _e46.regular;
        }
        s = this.getMonthCode(n, i);
        return {
          year: n,
          month: i,
          day: l,
          era: void 0,
          eraYear: a,
          monthCode: s
        };
      }
      if (this.validateCalendarDate(e), void 0 === i) {
        if (s.endsWith("L")) {
          if ("M05L" !== s) throw new RangeError("Hebrew leap month must have monthCode M05L, not ".concat(s));
          if (i = 6, !this.inLeapYear({
            year: n
          })) {
            if ("reject" === r) throw new RangeError("Hebrew monthCode M05L is invalid in year ".concat(n, " which is not a leap year"));
            i = 6, s = "M06";
          }
        } else {
          i = monthCodeNumberPart(s), this.inLeapYear({
            year: n
          }) && i >= 6 && i++;
          var _e47 = this.monthsInYear({
            year: n
          });
          if (i < 1 || i > _e47) throw new RangeError("Invalid monthCode: ".concat(s));
        }
      } else if ("reject" === r ? (RejectToRange(i, 1, this.monthsInYear({
        year: n
      })), RejectToRange(l, 1, this.maximumMonthLength({
        year: n,
        month: i
      }))) : (i = ConstrainToRange(i, 1, this.monthsInYear({
        year: n
      })), l = ConstrainToRange(l, 1, this.maximumMonthLength({
        year: n,
        month: i
      }))), void 0 === s) s = this.getMonthCode(n, i);else {
        if (this.getMonthCode(n, i) !== s) throw new RangeError("monthCode ".concat(s, " doesn't correspond to month ").concat(i, " in Hebrew year ").concat(n));
      }
      return _objectSpread2(_objectSpread2({}, e), {}, {
        day: l,
        month: i,
        monthCode: s,
        year: n,
        eraYear: a
      });
    }
  }]);
  return HebrewHelper;
}(HelperBase);
var IslamicBaseHelper = /*#__PURE__*/function (_HelperBase2) {
  _inherits(IslamicBaseHelper, _HelperBase2);
  var _super2 = _createSuper(IslamicBaseHelper);
  function IslamicBaseHelper() {
    var _this4;
    _classCallCheck(this, IslamicBaseHelper);
    _this4 = _super2.apply(this, arguments), _this4.calendarType = "lunar", _this4.DAYS_PER_ISLAMIC_YEAR = 354 + 11 / 30, _this4.DAYS_PER_ISO_YEAR = 365.2425, _this4.constantEra = "ah";
    return _this4;
  }
  _createClass(IslamicBaseHelper, [{
    key: "inLeapYear",
    value: function inLeapYear(e, t) {
      return 30 === this.daysInMonth({
        year: e.year,
        month: 12,
        day: 1
      }, t);
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear() {
      return 12;
    }
  }, {
    key: "minimumMonthLength",
    value: function minimumMonthLength() {
      return 29;
    }
  }, {
    key: "maximumMonthLength",
    value: function maximumMonthLength() {
      return 30;
    }
  }, {
    key: "estimateIsoDate",
    value: function estimateIsoDate(e) {
      var _this$adjustCalendarD = this.adjustCalendarDate(e),
        t = _this$adjustCalendarD.year;
      return {
        year: St(t * this.DAYS_PER_ISLAMIC_YEAR / this.DAYS_PER_ISO_YEAR) + 622,
        month: 1,
        day: 1
      };
    }
  }]);
  return IslamicBaseHelper;
}(HelperBase);
var IslamicHelper = /*#__PURE__*/function (_IslamicBaseHelper) {
  _inherits(IslamicHelper, _IslamicBaseHelper);
  var _super3 = _createSuper(IslamicHelper);
  function IslamicHelper() {
    var _this5;
    _classCallCheck(this, IslamicHelper);
    _this5 = _super3.apply(this, arguments), _this5.id = "islamic";
    return _this5;
  }
  return _createClass(IslamicHelper);
}(IslamicBaseHelper);
var IslamicUmalquraHelper = /*#__PURE__*/function (_IslamicBaseHelper2) {
  _inherits(IslamicUmalquraHelper, _IslamicBaseHelper2);
  var _super4 = _createSuper(IslamicUmalquraHelper);
  function IslamicUmalquraHelper() {
    var _this6;
    _classCallCheck(this, IslamicUmalquraHelper);
    _this6 = _super4.apply(this, arguments), _this6.id = "islamic-umalqura";
    return _this6;
  }
  return _createClass(IslamicUmalquraHelper);
}(IslamicBaseHelper);
var IslamicTblaHelper = /*#__PURE__*/function (_IslamicBaseHelper3) {
  _inherits(IslamicTblaHelper, _IslamicBaseHelper3);
  var _super5 = _createSuper(IslamicTblaHelper);
  function IslamicTblaHelper() {
    var _this7;
    _classCallCheck(this, IslamicTblaHelper);
    _this7 = _super5.apply(this, arguments), _this7.id = "islamic-tbla";
    return _this7;
  }
  return _createClass(IslamicTblaHelper);
}(IslamicBaseHelper);
var IslamicCivilHelper = /*#__PURE__*/function (_IslamicBaseHelper4) {
  _inherits(IslamicCivilHelper, _IslamicBaseHelper4);
  var _super6 = _createSuper(IslamicCivilHelper);
  function IslamicCivilHelper() {
    var _this8;
    _classCallCheck(this, IslamicCivilHelper);
    _this8 = _super6.apply(this, arguments), _this8.id = "islamic-civil";
    return _this8;
  }
  return _createClass(IslamicCivilHelper);
}(IslamicBaseHelper);
var IslamicRgsaHelper = /*#__PURE__*/function (_IslamicBaseHelper5) {
  _inherits(IslamicRgsaHelper, _IslamicBaseHelper5);
  var _super7 = _createSuper(IslamicRgsaHelper);
  function IslamicRgsaHelper() {
    var _this9;
    _classCallCheck(this, IslamicRgsaHelper);
    _this9 = _super7.apply(this, arguments), _this9.id = "islamic-rgsa";
    return _this9;
  }
  return _createClass(IslamicRgsaHelper);
}(IslamicBaseHelper);
var IslamicCcHelper = /*#__PURE__*/function (_IslamicBaseHelper6) {
  _inherits(IslamicCcHelper, _IslamicBaseHelper6);
  var _super8 = _createSuper(IslamicCcHelper);
  function IslamicCcHelper() {
    var _this10;
    _classCallCheck(this, IslamicCcHelper);
    _this10 = _super8.apply(this, arguments), _this10.id = "islamicc";
    return _this10;
  }
  return _createClass(IslamicCcHelper);
}(IslamicBaseHelper);
var PersianHelper = /*#__PURE__*/function (_HelperBase3) {
  _inherits(PersianHelper, _HelperBase3);
  var _super9 = _createSuper(PersianHelper);
  function PersianHelper() {
    var _this11;
    _classCallCheck(this, PersianHelper);
    _this11 = _super9.apply(this, arguments), _this11.id = "persian", _this11.calendarType = "solar", _this11.constantEra = "ap";
    return _this11;
  }
  _createClass(PersianHelper, [{
    key: "inLeapYear",
    value: function inLeapYear(e, t) {
      return IslamicHelper.prototype.inLeapYear.call(this, e, t);
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear() {
      return 12;
    }
  }, {
    key: "minimumMonthLength",
    value: function minimumMonthLength(e) {
      var t = e.month;
      return 12 === t ? 29 : t <= 6 ? 31 : 30;
    }
  }, {
    key: "maximumMonthLength",
    value: function maximumMonthLength(e) {
      var t = e.month;
      return 12 === t ? 30 : t <= 6 ? 31 : 30;
    }
  }, {
    key: "estimateIsoDate",
    value: function estimateIsoDate(e) {
      var _this$adjustCalendarD2 = this.adjustCalendarDate(e),
        t = _this$adjustCalendarD2.year;
      return {
        year: t + 621,
        month: 1,
        day: 1
      };
    }
  }]);
  return PersianHelper;
}(HelperBase);
var IndianHelper = /*#__PURE__*/function (_HelperBase4) {
  _inherits(IndianHelper, _HelperBase4);
  var _super10 = _createSuper(IndianHelper);
  function IndianHelper() {
    var _this12;
    _classCallCheck(this, IndianHelper);
    _this12 = _super10.apply(this, arguments), _this12.id = "indian", _this12.calendarType = "solar", _this12.constantEra = "saka", _this12.months = {
      1: {
        length: 30,
        month: 3,
        day: 22,
        leap: {
          length: 31,
          month: 3,
          day: 21
        }
      },
      2: {
        length: 31,
        month: 4,
        day: 21
      },
      3: {
        length: 31,
        month: 5,
        day: 22
      },
      4: {
        length: 31,
        month: 6,
        day: 22
      },
      5: {
        length: 31,
        month: 7,
        day: 23
      },
      6: {
        length: 31,
        month: 8,
        day: 23
      },
      7: {
        length: 30,
        month: 9,
        day: 23
      },
      8: {
        length: 30,
        month: 10,
        day: 23
      },
      9: {
        length: 30,
        month: 11,
        day: 22
      },
      10: {
        length: 30,
        month: 12,
        day: 22
      },
      11: {
        length: 30,
        month: 1,
        nextYear: !0,
        day: 21
      },
      12: {
        length: 30,
        month: 2,
        nextYear: !0,
        day: 20
      }
    }, _this12.vulnerableToBceBug = "10/11/-79 Saka" !== new Date("0000-01-01T00:00Z").toLocaleDateString("en-US-u-ca-indian", {
      timeZone: "UTC"
    });
    return _this12;
  }
  _createClass(IndianHelper, [{
    key: "inLeapYear",
    value: function inLeapYear(e) {
      return isGregorianLeapYear(e.year + 78);
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear() {
      return 12;
    }
  }, {
    key: "minimumMonthLength",
    value: function minimumMonthLength(e) {
      return this.getMonthInfo(e).length;
    }
  }, {
    key: "maximumMonthLength",
    value: function maximumMonthLength(e) {
      return this.getMonthInfo(e).length;
    }
  }, {
    key: "getMonthInfo",
    value: function getMonthInfo(e) {
      var t = e.month;
      var r = this.months[t];
      if (void 0 === r) throw new RangeError("Invalid month: ".concat(t));
      return this.inLeapYear(e) && r.leap && (r = r.leap), r;
    }
  }, {
    key: "estimateIsoDate",
    value: function estimateIsoDate(e) {
      var t = this.adjustCalendarDate(e),
        r = this.getMonthInfo(t);
      return AddISODate(t.year + 78 + (r.nextYear ? 1 : 0), r.month, r.day, 0, 0, 0, t.day - 1, "constrain");
    }
  }, {
    key: "checkIcuBugs",
    value: function checkIcuBugs(e) {
      if (this.vulnerableToBceBug && e.year < 1) throw new RangeError("calendar '".concat(this.id, "' is broken for ISO dates before 0001-01-01 (see https://bugs.chromium.org/p/v8/issues/detail?id=10529)"));
    }
  }]);
  return IndianHelper;
}(HelperBase);
function isGregorianLeapYear(e) {
  return e % 4 == 0 && (e % 100 != 0 || e % 400 == 0);
}
var GregorianBaseHelper = /*#__PURE__*/function (_HelperBase5) {
  _inherits(GregorianBaseHelper, _HelperBase5);
  var _super11 = _createSuper(GregorianBaseHelper);
  function GregorianBaseHelper(e, t) {
    var _this13;
    _classCallCheck(this, GregorianBaseHelper);
    _this13 = _super11.call(this), _this13.calendarType = "solar", _this13.v8IsVulnerableToJulianBug = new Date("+001001-01-01T00:00Z").toLocaleDateString("en-US-u-ca-japanese", {
      timeZone: "UTC"
    }).startsWith("12"), _this13.calendarIsVulnerableToJulianBug = !1, _this13.id = e;
    var _adjustEras = function adjustEras(e) {
        var t,
          r = e;
        if (0 === r.length) throw new RangeError("Invalid era data: eras are required");
        if (1 === r.length && r[0].reverseOf) throw new RangeError("Invalid era data: anchor era cannot count years backwards");
        if (1 === r.length && !r[0].name) throw new RangeError("Invalid era data: at least one named era is required");
        if (r.filter(function (e) {
          return null != e.reverseOf;
        }).length > 1) throw new RangeError("Invalid era data: only one era can count years backwards");
        r.forEach(function (e) {
          if (e.isAnchor || !e.anchorEpoch && !e.reverseOf) {
            if (t) throw new RangeError("Invalid era data: cannot have multiple anchor eras");
            t = e, e.anchorEpoch = {
              year: e.hasYearZero ? 0 : 1
            };
          } else if (!e.name) throw new RangeError("If era name is blank, it must be the anchor era");
        }), r = r.filter(function (e) {
          return e.name;
        }), r.forEach(function (e) {
          var t = e.reverseOf;
          if (t) {
            var _o38 = r.find(function (e) {
              return e.name === t;
            });
            if (void 0 === _o38) throw new RangeError("Invalid era data: unmatched reverseOf era: ".concat(t));
            e.reverseOf = _o38, e.anchorEpoch = _o38.anchorEpoch, e.isoEpoch = _o38.isoEpoch;
          }
          void 0 === e.anchorEpoch.month && (e.anchorEpoch.month = 1), void 0 === e.anchorEpoch.day && (e.anchorEpoch.day = 1);
        }), yt.call(r, function (e, t) {
          if (e.reverseOf) return 1;
          if (t.reverseOf) return -1;
          if (!e.isoEpoch || !t.isoEpoch) throw new RangeError("Invalid era data: missing ISO epoch");
          return t.isoEpoch.year - e.isoEpoch.year;
        });
        var o = r[r.length - 1].reverseOf;
        if (o && o !== r[r.length - 2]) throw new RangeError("Invalid era data: invalid reverse-sign era");
        return r.forEach(function (e, t) {
          e.genericName = "era" + (r.length - 1 - t);
        }), {
          eras: r,
          anchorEra: t || r[0]
        };
      }(t),
      r = _adjustEras.eras,
      o = _adjustEras.anchorEra;
    _this13.anchorEra = o, _this13.eras = r;
    return _this13;
  }
  _createClass(GregorianBaseHelper, [{
    key: "inLeapYear",
    value: function inLeapYear(e) {
      var _this$estimateIsoDate = this.estimateIsoDate({
          month: 1,
          day: 1,
          year: e.year
        }),
        t = _this$estimateIsoDate.year;
      return isGregorianLeapYear(t);
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear() {
      return 12;
    }
  }, {
    key: "minimumMonthLength",
    value: function minimumMonthLength(e) {
      var t = e.month;
      return 2 === t ? this.inLeapYear(e) ? 29 : 28 : [4, 6, 9, 11].indexOf(t) >= 0 ? 30 : 31;
    }
  }, {
    key: "maximumMonthLength",
    value: function maximumMonthLength(e) {
      return this.minimumMonthLength(e);
    }
  }, {
    key: "completeEraYear",
    value: function completeEraYear(e) {
      var _this14 = this,
        _eraFromYear;
      var checkField = function checkField(t, r) {
          var o = e[t];
          if (null != o && o != r) throw new RangeError("Input ".concat(t, " ").concat(o, " doesn't match calculated value ").concat(r));
        },
        eraFromYear = function eraFromYear(t) {
          var r;
          var o = _objectSpread2(_objectSpread2({}, e), {}, {
              year: t
            }),
            n = _this14.eras.find(function (e, n) {
              if (n === _this14.eras.length - 1) {
                if (e.reverseOf) {
                  if (t > 0) throw new RangeError("Signed year ".concat(t, " is invalid for era ").concat(e.name));
                  return r = e.anchorEpoch.year - t, !0;
                }
                return r = t - e.anchorEpoch.year + (e.hasYearZero ? 0 : 1), !0;
              }
              return _this14.compareCalendarDates(o, e.anchorEpoch) >= 0 && (r = t - e.anchorEpoch.year + (e.hasYearZero ? 0 : 1), !0);
            });
          if (!n) throw new RangeError("Year ".concat(t, " was not matched by any era"));
          return {
            eraYear: r,
            era: n.name
          };
        };
      var t = e.year,
        r = e.eraYear,
        o = e.era;
      if (null != t) (_eraFromYear = eraFromYear(t), r = _eraFromYear.eraYear, o = _eraFromYear.era), checkField("era", o), checkField("eraYear", r);else {
        if (null == r) throw new RangeError("Either `year` or `eraYear` and `era` are required");
        {
          var _eraFromYear2;
          var _e48 = void 0 === o ? void 0 : this.eras.find(function (e) {
            return e.name === o || e.genericName === o;
          });
          if (!_e48) throw new RangeError("Era ".concat(o, " (ISO year ").concat(r, ") was not matched by any era"));
          if (r < 1 && _e48.reverseOf) throw new RangeError("Years in ".concat(o, " era must be positive, not ").concat(t));
          t = _e48.reverseOf ? _e48.anchorEpoch.year - r : r + _e48.anchorEpoch.year - (_e48.hasYearZero ? 0 : 1), checkField("year", t), (_eraFromYear2 = eraFromYear(t), r = _eraFromYear2.eraYear, o = _eraFromYear2.era);
        }
      }
      return _objectSpread2(_objectSpread2({}, e), {}, {
        year: t,
        eraYear: r,
        era: o
      });
    }
  }, {
    key: "adjustCalendarDate",
    value: function adjustCalendarDate(e, t) {
      var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "constrain";
      var o = e;
      var _o39 = o,
        n = _o39.month,
        a = _o39.monthCode;
      return void 0 === n && (o = _objectSpread2(_objectSpread2({}, o), {}, {
        month: monthCodeNumberPart(a)
      })), this.validateCalendarDate(o), o = this.completeEraYear(o), _get(_getPrototypeOf(GregorianBaseHelper.prototype), "adjustCalendarDate", this).call(this, o, t, r);
    }
  }, {
    key: "estimateIsoDate",
    value: function estimateIsoDate(e) {
      var t = this.adjustCalendarDate(e),
        r = t.year,
        o = t.month,
        n = t.day,
        a = this.anchorEra;
      return RegulateISODate(r + a.isoEpoch.year - (a.hasYearZero ? 0 : 1), o, n, "constrain");
    }
  }, {
    key: "checkIcuBugs",
    value: function checkIcuBugs(e) {
      if (this.calendarIsVulnerableToJulianBug && this.v8IsVulnerableToJulianBug) {
        if (CompareISODate(e.year, e.month, e.day, 1582, 10, 15) < 0) throw new RangeError("calendar '".concat(this.id, "' is broken for ISO dates before 1582-10-15 (see https://bugs.chromium.org/p/chromium/issues/detail?id=1173158)"));
      }
    }
  }]);
  return GregorianBaseHelper;
}(HelperBase);
var OrthodoxBaseHelper = /*#__PURE__*/function (_GregorianBaseHelper) {
  _inherits(OrthodoxBaseHelper, _GregorianBaseHelper);
  var _super12 = _createSuper(OrthodoxBaseHelper);
  function OrthodoxBaseHelper(e, t) {
    _classCallCheck(this, OrthodoxBaseHelper);
    return _super12.call(this, e, t);
  }
  _createClass(OrthodoxBaseHelper, [{
    key: "inLeapYear",
    value: function inLeapYear(e) {
      var t = e.year;
      return (t + 1) % 4 == 0;
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear() {
      return 13;
    }
  }, {
    key: "minimumMonthLength",
    value: function minimumMonthLength(e) {
      var t = e.month;
      return 13 === t ? this.inLeapYear(e) ? 6 : 5 : 30;
    }
  }, {
    key: "maximumMonthLength",
    value: function maximumMonthLength(e) {
      return this.minimumMonthLength(e);
    }
  }]);
  return OrthodoxBaseHelper;
}(GregorianBaseHelper);
var EthioaaHelper = /*#__PURE__*/function (_OrthodoxBaseHelper) {
  _inherits(EthioaaHelper, _OrthodoxBaseHelper);
  var _super13 = _createSuper(EthioaaHelper);
  function EthioaaHelper() {
    _classCallCheck(this, EthioaaHelper);
    return _super13.call(this, "ethioaa", [{
      name: "era0",
      isoEpoch: {
        year: -5492,
        month: 7,
        day: 17
      }
    }]);
  }
  return _createClass(EthioaaHelper);
}(OrthodoxBaseHelper);
var CopticHelper = /*#__PURE__*/function (_OrthodoxBaseHelper2) {
  _inherits(CopticHelper, _OrthodoxBaseHelper2);
  var _super14 = _createSuper(CopticHelper);
  function CopticHelper() {
    _classCallCheck(this, CopticHelper);
    return _super14.call(this, "coptic", [{
      name: "era1",
      isoEpoch: {
        year: 284,
        month: 8,
        day: 29
      }
    }, {
      name: "era0",
      reverseOf: "era1"
    }]);
  }
  return _createClass(CopticHelper);
}(OrthodoxBaseHelper);
var EthiopicHelper = /*#__PURE__*/function (_OrthodoxBaseHelper3) {
  _inherits(EthiopicHelper, _OrthodoxBaseHelper3);
  var _super15 = _createSuper(EthiopicHelper);
  function EthiopicHelper() {
    _classCallCheck(this, EthiopicHelper);
    return _super15.call(this, "ethiopic", [{
      name: "era0",
      isoEpoch: {
        year: -5492,
        month: 7,
        day: 17
      }
    }, {
      name: "era1",
      isoEpoch: {
        year: 8,
        month: 8,
        day: 27
      },
      anchorEpoch: {
        year: 5501
      }
    }]);
  }
  return _createClass(EthiopicHelper);
}(OrthodoxBaseHelper);
var RocHelper = /*#__PURE__*/function (_GregorianBaseHelper2) {
  _inherits(RocHelper, _GregorianBaseHelper2);
  var _super16 = _createSuper(RocHelper);
  function RocHelper() {
    var _this15;
    _classCallCheck(this, RocHelper);
    _this15 = _super16.call(this, "roc", [{
      name: "minguo",
      isoEpoch: {
        year: 1912,
        month: 1,
        day: 1
      }
    }, {
      name: "before-roc",
      reverseOf: "minguo"
    }]), _this15.calendarIsVulnerableToJulianBug = !0;
    return _this15;
  }
  return _createClass(RocHelper);
}(GregorianBaseHelper);
var BuddhistHelper = /*#__PURE__*/function (_GregorianBaseHelper3) {
  _inherits(BuddhistHelper, _GregorianBaseHelper3);
  var _super17 = _createSuper(BuddhistHelper);
  function BuddhistHelper() {
    var _this16;
    _classCallCheck(this, BuddhistHelper);
    _this16 = _super17.call(this, "buddhist", [{
      name: "be",
      hasYearZero: !0,
      isoEpoch: {
        year: -543,
        month: 1,
        day: 1
      }
    }]), _this16.calendarIsVulnerableToJulianBug = !0;
    return _this16;
  }
  return _createClass(BuddhistHelper);
}(GregorianBaseHelper);
var GregoryHelper = /*#__PURE__*/function (_GregorianBaseHelper4) {
  _inherits(GregoryHelper, _GregorianBaseHelper4);
  var _super18 = _createSuper(GregoryHelper);
  function GregoryHelper() {
    _classCallCheck(this, GregoryHelper);
    return _super18.call(this, "gregory", [{
      name: "ce",
      isoEpoch: {
        year: 1,
        month: 1,
        day: 1
      }
    }, {
      name: "bce",
      reverseOf: "ce"
    }]);
  }
  _createClass(GregoryHelper, [{
    key: "reviseIntlEra",
    value: function reviseIntlEra(e) {
      var t = e.era,
        r = e.eraYear;
      return "bc" !== t && "b" !== t || (t = "bce"), "ad" !== t && "a" !== t || (t = "ce"), {
        era: t,
        eraYear: r
      };
    }
  }]);
  return GregoryHelper;
}(GregorianBaseHelper);
var JapaneseHelper = /*#__PURE__*/function (_GregorianBaseHelper5) {
  _inherits(JapaneseHelper, _GregorianBaseHelper5);
  var _super19 = _createSuper(JapaneseHelper);
  function JapaneseHelper() {
    var _this17;
    _classCallCheck(this, JapaneseHelper);
    _this17 = _super19.call(this, "japanese", [{
      name: "reiwa",
      isoEpoch: {
        year: 2019,
        month: 5,
        day: 1
      },
      anchorEpoch: {
        year: 2019,
        month: 5,
        day: 1
      }
    }, {
      name: "heisei",
      isoEpoch: {
        year: 1989,
        month: 1,
        day: 8
      },
      anchorEpoch: {
        year: 1989,
        month: 1,
        day: 8
      }
    }, {
      name: "showa",
      isoEpoch: {
        year: 1926,
        month: 12,
        day: 25
      },
      anchorEpoch: {
        year: 1926,
        month: 12,
        day: 25
      }
    }, {
      name: "taisho",
      isoEpoch: {
        year: 1912,
        month: 7,
        day: 30
      },
      anchorEpoch: {
        year: 1912,
        month: 7,
        day: 30
      }
    }, {
      name: "meiji",
      isoEpoch: {
        year: 1868,
        month: 9,
        day: 8
      },
      anchorEpoch: {
        year: 1868,
        month: 9,
        day: 8
      }
    }, {
      name: "ce",
      isoEpoch: {
        year: 1,
        month: 1,
        day: 1
      }
    }, {
      name: "bce",
      reverseOf: "ce"
    }]), _this17.calendarIsVulnerableToJulianBug = !0, _this17.eraLength = "long", _this17.erasBeginMidYear = !0;
    return _this17;
  }
  _createClass(JapaneseHelper, [{
    key: "reviseIntlEra",
    value: function reviseIntlEra(e, t) {
      var r = e.era,
        o = e.eraYear,
        n = t.year;
      return this.eras.find(function (e) {
        return e.name === r;
      }) ? {
        era: r,
        eraYear: o
      } : n < 1 ? {
        era: "bce",
        eraYear: 1 - n
      } : {
        era: "ce",
        eraYear: n
      };
    }
  }]);
  return JapaneseHelper;
}(GregorianBaseHelper);
var ChineseBaseHelper = /*#__PURE__*/function (_HelperBase6) {
  _inherits(ChineseBaseHelper, _HelperBase6);
  var _super20 = _createSuper(ChineseBaseHelper);
  function ChineseBaseHelper() {
    var _this18;
    _classCallCheck(this, ChineseBaseHelper);
    _this18 = _super20.apply(this, arguments), _this18.calendarType = "lunisolar", _this18.hasEra = !1;
    return _this18;
  }
  _createClass(ChineseBaseHelper, [{
    key: "inLeapYear",
    value: function inLeapYear(e, t) {
      var r = this.getMonthList(e.year, t);
      return 13 === wt(r).length;
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear(e, t) {
      return this.inLeapYear(e, t) ? 13 : 12;
    }
  }, {
    key: "minimumMonthLength",
    value: function minimumMonthLength() {
      return 29;
    }
  }, {
    key: "maximumMonthLength",
    value: function maximumMonthLength() {
      return 30;
    }
  }, {
    key: "getMonthList",
    value: function getMonthList(e, t) {
      var _this19 = this,
        _getCalendarDate2;
      if (void 0 === e) throw new TypeError("Missing year");
      var r = JSON.stringify({
          func: "getMonthList",
          calendarYear: e,
          id: this.id
        }),
        o = t.get(r);
      if (o) return o;
      var n = this.getFormatter(),
        getCalendarDate = function getCalendarDate(e, t) {
          var r = toUtcIsoDateString({
              isoYear: e,
              isoMonth: 2,
              isoDay: 1
            }),
            o = new Date(r);
          o.setUTCDate(t + 1);
          var a = n.formatToParts(o),
            i = a.find(function (e) {
              return "month" === e.type;
            }).value,
            s = +a.find(function (e) {
              return "day" === e.type;
            }).value;
          var l = a.find(function (e) {
            return "relatedYear" === e.type;
          });
          if (void 0 === l) throw new RangeError("Intl.DateTimeFormat.formatToParts lacks relatedYear in ".concat(_this19.id, " calendar. Try Node 14+ or modern browsers."));
          return l = +l.value, {
            calendarMonthString: i,
            calendarDay: s,
            calendarYearToVerify: l
          };
        };
      var a = 17,
        _getCalendarDate = getCalendarDate(e, a),
        i = _getCalendarDate.calendarMonthString,
        s = _getCalendarDate.calendarDay,
        l = _getCalendarDate.calendarYearToVerify;
      "1" !== i && (a += 29, (_getCalendarDate2 = getCalendarDate(e, a), i = _getCalendarDate2.calendarMonthString, s = _getCalendarDate2.calendarDay)), a -= s - 5;
      var d = {};
      var m,
        c,
        h = 1,
        u = !1;
      do {
        var _getCalendarDate3;
        (_getCalendarDate3 = getCalendarDate(e, a), i = _getCalendarDate3.calendarMonthString, s = _getCalendarDate3.calendarDay, l = _getCalendarDate3.calendarYearToVerify), m && (d[c].daysInMonth = m + 30 - s), l !== e ? u = !0 : (d[i] = {
          monthIndex: h++
        }, a += 30), m = s, c = i;
      } while (!u);
      return d[c].daysInMonth = m + 30 - s, t.set(r, d), d;
    }
  }, {
    key: "estimateIsoDate",
    value: function estimateIsoDate(e) {
      var t = e.year,
        r = e.month;
      return {
        year: t,
        month: r >= 12 ? 12 : r + 1,
        day: 1
      };
    }
  }, {
    key: "adjustCalendarDate",
    value: function adjustCalendarDate(e, t) {
      var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "constrain";
      var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : !1;
      var n = e.year,
        a = e.month,
        i = e.monthExtra,
        s = e.day,
        l = e.monthCode,
        d = e.eraYear;
      if (o) {
        if (n = d, i && "bis" !== i) throw new RangeError("Unexpected leap month suffix: ".concat(i));
        var _e49 = buildMonthCode(a, void 0 !== i),
          _r65 = "".concat(a).concat(i || ""),
          _o40 = this.getMonthList(n, t)[_r65];
        if (void 0 === _o40) throw new RangeError("Unmatched month ".concat(_r65, " in Chinese year ").concat(n));
        return a = _o40.monthIndex, {
          year: n,
          month: a,
          day: s,
          era: void 0,
          eraYear: d,
          monthCode: _e49
        };
      }
      if (this.validateCalendarDate(e), void 0 === n && (n = d), void 0 === d && (d = n), void 0 === a) {
        var _e50 = this.getMonthList(n, t);
        var _o41 = l.replace("L", "bis").slice(1);
        "0" === _o41[0] && (_o41 = _o41.slice(1));
        var _i19 = _e50[_o41];
        if (a = _i19 && _i19.monthIndex, void 0 === a && l.endsWith("L") && "M13L" != l && "constrain" === r) {
          var _t48 = l.slice(1, -1);
          "0" === _t48[0] && (_t48 = _t48.slice(1)), _i19 = _e50[_t48], _i19 && (a = _i19.monthIndex, l = buildMonthCode(_t48));
        }
        if (void 0 === a) throw new RangeError("Unmatched month ".concat(l, " in Chinese year ").concat(n));
      } else if (void 0 === l) {
        var _e51 = this.getMonthList(n, t),
          _o42 = wt(_e51),
          _i20 = _o42.length;
        "reject" === r ? (RejectToRange(a, 1, _i20), RejectToRange(s, 1, this.maximumMonthLength())) : (a = ConstrainToRange(a, 1, _i20), s = ConstrainToRange(s, 1, this.maximumMonthLength()));
        var _d11 = _o42.find(function (_ref10) {
          var _ref11 = _slicedToArray(_ref10, 2),
            e = _ref11[1];
          return e.monthIndex === a;
        });
        if (void 0 === _d11) throw new RangeError("Invalid month ".concat(a, " in Chinese year ").concat(n));
        l = buildMonthCode(_d11[0].replace("bis", ""), -1 !== _d11[0].indexOf("bis"));
      } else {
        var _e52 = this.getMonthList(n, t);
        var _r66 = l.replace("L", "bis").slice(1);
        "0" === _r66[0] && (_r66 = _r66.slice(1));
        var _o43 = _e52[_r66];
        if (!_o43) throw new RangeError("Unmatched monthCode ".concat(l, " in Chinese year ").concat(n));
        if (a !== _o43.monthIndex) throw new RangeError("monthCode ".concat(l, " doesn't correspond to month ").concat(a, " in Chinese year ").concat(n));
      }
      return _objectSpread2(_objectSpread2({}, e), {}, {
        year: n,
        eraYear: d,
        month: a,
        monthCode: l,
        day: s
      });
    }
  }]);
  return ChineseBaseHelper;
}(HelperBase);
var ChineseHelper = /*#__PURE__*/function (_ChineseBaseHelper) {
  _inherits(ChineseHelper, _ChineseBaseHelper);
  var _super21 = _createSuper(ChineseHelper);
  function ChineseHelper() {
    var _this20;
    _classCallCheck(this, ChineseHelper);
    _this20 = _super21.apply(this, arguments), _this20.id = "chinese";
    return _this20;
  }
  return _createClass(ChineseHelper);
}(ChineseBaseHelper);
var DangiHelper = /*#__PURE__*/function (_ChineseBaseHelper2) {
  _inherits(DangiHelper, _ChineseBaseHelper2);
  var _super22 = _createSuper(DangiHelper);
  function DangiHelper() {
    var _this21;
    _classCallCheck(this, DangiHelper);
    _this21 = _super22.apply(this, arguments), _this21.id = "dangi";
    return _this21;
  }
  return _createClass(DangiHelper);
}(ChineseBaseHelper);
var NonIsoCalendar = /*#__PURE__*/function () {
  function NonIsoCalendar(e) {
    _classCallCheck(this, NonIsoCalendar);
    this.helper = e;
  }
  _createClass(NonIsoCalendar, [{
    key: "dateFromFields",
    value: function dateFromFields(e, t, r) {
      var o = new OneObjectCache(),
        n = PrepareTemporalFields(e, this.fields(["day", "month", "monthCode", "year"]), []),
        a = ToTemporalOverflow(t),
        _this$helper$calendar = this.helper.calendarToIsoDate(n, a, o),
        i = _this$helper$calendar.year,
        s = _this$helper$calendar.month,
        l = _this$helper$calendar.day,
        d = CreateTemporalDate(i, s, l, r);
      return o.setObject(d), d;
    }
  }, {
    key: "yearMonthFromFields",
    value: function yearMonthFromFields(e, t, r) {
      var o = new OneObjectCache(),
        n = PrepareTemporalFields(e, this.fields(["month", "monthCode", "year"]), []),
        a = ToTemporalOverflow(t),
        _this$helper$calendar2 = this.helper.calendarToIsoDate(_objectSpread2(_objectSpread2({}, n), {}, {
          day: 1
        }), a, o),
        i = _this$helper$calendar2.year,
        s = _this$helper$calendar2.month,
        l = _this$helper$calendar2.day,
        d = CreateTemporalYearMonth(i, s, r, l);
      return o.setObject(d), d;
    }
  }, {
    key: "monthDayFromFields",
    value: function monthDayFromFields(e, t, r) {
      var o = new OneObjectCache(),
        n = PrepareTemporalFields(e, this.fields(["day", "month", "monthCode", "year"]), []),
        a = ToTemporalOverflow(t),
        _this$helper$monthDay = this.helper.monthDayFromFields(n, a, o),
        i = _this$helper$monthDay.year,
        s = _this$helper$monthDay.month,
        l = _this$helper$monthDay.day,
        d = CreateTemporalMonthDay(s, l, r, i);
      return o.setObject(d), d;
    }
  }, {
    key: "fields",
    value: function fields(e) {
      var t = e;
      return Tt.call(t, "year") && (t = [].concat(_toConsumableArray(t), ["era", "eraYear"])), t;
    }
  }, {
    key: "fieldKeysToIgnore",
    value: function fieldKeysToIgnore(e) {
      var t = new Dt();
      for (var _r67 = 0; _r67 < e.length; _r67++) {
        var _o44 = e[_r67];
        switch (Call(vt, t, [_o44]), _o44) {
          case "era":
            Call(vt, t, ["eraYear"]), Call(vt, t, ["year"]);
            break;
          case "eraYear":
            Call(vt, t, ["era"]), Call(vt, t, ["year"]);
            break;
          case "year":
            Call(vt, t, ["era"]), Call(vt, t, ["eraYear"]);
            break;
          case "month":
            Call(vt, t, ["monthCode"]), this.helper.erasBeginMidYear && (Call(vt, t, ["era"]), Call(vt, t, ["eraYear"]));
            break;
          case "monthCode":
            Call(vt, t, ["month"]), this.helper.erasBeginMidYear && (Call(vt, t, ["era"]), Call(vt, t, ["eraYear"]));
            break;
          case "day":
            this.helper.erasBeginMidYear && (Call(vt, t, ["era"]), Call(vt, t, ["eraYear"]));
        }
      }
      return _toConsumableArray(Call(Ct, t, []));
    }
  }, {
    key: "dateAdd",
    value: function dateAdd(e, t, r, o, n, a, i) {
      var s = OneObjectCache.getCacheForObject(e),
        l = this.helper.temporalToCalendarDate(e, s),
        d = this.helper.addCalendar(l, {
          years: t,
          months: r,
          weeks: o,
          days: n
        }, a, s),
        m = this.helper.calendarToIsoDate(d, "constrain", s),
        c = m.year,
        h = m.month,
        u = m.day,
        T = CreateTemporalDate(c, h, u, i);
      return new OneObjectCache(s).setObject(T), T;
    }
  }, {
    key: "dateUntil",
    value: function dateUntil(e, t, r) {
      var o = OneObjectCache.getCacheForObject(e),
        n = OneObjectCache.getCacheForObject(t),
        a = this.helper.temporalToCalendarDate(e, o),
        i = this.helper.temporalToCalendarDate(t, n);
      return this.helper.untilCalendar(a, i, r, o);
    }
  }, {
    key: "year",
    value: function year(e) {
      var t = OneObjectCache.getCacheForObject(e);
      return this.helper.temporalToCalendarDate(e, t).year;
    }
  }, {
    key: "month",
    value: function month(e) {
      var t = OneObjectCache.getCacheForObject(e);
      return this.helper.temporalToCalendarDate(e, t).month;
    }
  }, {
    key: "day",
    value: function day(e) {
      var t = OneObjectCache.getCacheForObject(e);
      return this.helper.temporalToCalendarDate(e, t).day;
    }
  }, {
    key: "era",
    value: function era(e) {
      if (!this.helper.hasEra) return;
      var t = OneObjectCache.getCacheForObject(e);
      return this.helper.temporalToCalendarDate(e, t).era;
    }
  }, {
    key: "eraYear",
    value: function eraYear(e) {
      if (!this.helper.hasEra) return;
      var t = OneObjectCache.getCacheForObject(e);
      return this.helper.temporalToCalendarDate(e, t).eraYear;
    }
  }, {
    key: "monthCode",
    value: function monthCode(e) {
      var t = OneObjectCache.getCacheForObject(e);
      return this.helper.temporalToCalendarDate(e, t).monthCode;
    }
  }, {
    key: "dayOfWeek",
    value: function dayOfWeek(e) {
      return Ot.iso8601.dayOfWeek(e);
    }
  }, {
    key: "dayOfYear",
    value: function dayOfYear(e) {
      var t = OneObjectCache.getCacheForObject(e),
        r = this.helper.isoToCalendarDate(e, t),
        o = this.helper.startOfCalendarYear(r);
      return this.helper.calendarDaysUntil(o, r, t) + 1;
    }
  }, {
    key: "weekOfYear",
    value: function weekOfYear(e) {
      return Ot.iso8601.weekOfYear(e);
    }
  }, {
    key: "yearOfWeek",
    value: function yearOfWeek(e) {
      return Ot.iso8601.yearOfWeek(e);
    }
  }, {
    key: "daysInWeek",
    value: function daysInWeek(e) {
      return Ot.iso8601.daysInWeek(e);
    }
  }, {
    key: "daysInMonth",
    value: function daysInMonth(e) {
      var t = OneObjectCache.getCacheForObject(e),
        r = this.helper.temporalToCalendarDate(e, t),
        o = this.helper.maximumMonthLength(r);
      if (o === this.helper.minimumMonthLength(r)) return o;
      var n = this.helper.startOfCalendarMonth(r),
        a = this.helper.addMonthsCalendar(n, 1, "constrain", t);
      return this.helper.calendarDaysUntil(n, a, t);
    }
  }, {
    key: "daysInYear",
    value: function daysInYear(e) {
      var t = e;
      HasSlot(t, i) || (t = ToTemporalDate(t));
      var r = OneObjectCache.getCacheForObject(t),
        o = this.helper.temporalToCalendarDate(t, r),
        n = this.helper.startOfCalendarYear(o),
        a = this.helper.addCalendar(n, {
          years: 1
        }, "constrain", r);
      return this.helper.calendarDaysUntil(n, a, r);
    }
  }, {
    key: "monthsInYear",
    value: function monthsInYear(e) {
      var t = OneObjectCache.getCacheForObject(e),
        r = this.helper.temporalToCalendarDate(e, t);
      return this.helper.monthsInYear(r, t);
    }
  }, {
    key: "inLeapYear",
    value: function inLeapYear(e) {
      var t = e;
      HasSlot(t, i) || (t = ToTemporalDate(t));
      var r = OneObjectCache.getCacheForObject(t),
        o = this.helper.temporalToCalendarDate(t, r);
      return this.helper.inLeapYear(o, r);
    }
  }]);
  return NonIsoCalendar;
}();
for (var _i21 = 0, _arr7 = [HebrewHelper, PersianHelper, EthiopicHelper, EthioaaHelper, CopticHelper, ChineseHelper, DangiHelper, RocHelper, IndianHelper, BuddhistHelper, GregoryHelper, JapaneseHelper, IslamicHelper, IslamicUmalquraHelper, IslamicTblaHelper, IslamicCivilHelper, IslamicRgsaHelper, IslamicCcHelper]; _i21 < _arr7.length; _i21++) {
  var _e53 = _arr7[_i21];
  var _t49 = new _e53();
  Ot[_t49.id] = new NonIsoCalendar(_t49);
}
var PlainDate = /*#__PURE__*/function () {
  function PlainDate(e, t, r) {
    var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : "iso8601";
    _classCallCheck(this, PlainDate);
    CreateTemporalDateSlots(this, ToIntegerWithTruncation(e), ToIntegerWithTruncation(t), ToIntegerWithTruncation(r), ToTemporalCalendarSlotValue(o));
  }
  _createClass(PlainDate, [{
    key: "calendarId",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarIdentifier(GetSlot(this, p));
    }
  }, {
    key: "era",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarEra(GetSlot(this, p), this);
    }
  }, {
    key: "eraYear",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarEraYear(GetSlot(this, p), this);
    }
  }, {
    key: "year",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarYear(GetSlot(this, p), this);
    }
  }, {
    key: "month",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarMonth(GetSlot(this, p), this);
    }
  }, {
    key: "monthCode",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarMonthCode(GetSlot(this, p), this);
    }
  }, {
    key: "day",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarDay(GetSlot(this, p), this);
    }
  }, {
    key: "dayOfWeek",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarDayOfWeek(GetSlot(this, p), this);
    }
  }, {
    key: "dayOfYear",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarDayOfYear(GetSlot(this, p), this);
    }
  }, {
    key: "weekOfYear",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarWeekOfYear(GetSlot(this, p), this);
    }
  }, {
    key: "yearOfWeek",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarYearOfWeek(GetSlot(this, p), this);
    }
  }, {
    key: "daysInWeek",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInWeek(GetSlot(this, p), this);
    }
  }, {
    key: "daysInMonth",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInMonth(GetSlot(this, p), this);
    }
  }, {
    key: "daysInYear",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInYear(GetSlot(this, p), this);
    }
  }, {
    key: "monthsInYear",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarMonthsInYear(GetSlot(this, p), this);
    }
  }, {
    key: "inLeapYear",
    get: function get() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return CalendarInLeapYear(GetSlot(this, p), this);
    }
  }, {
    key: "with",
    value: function _with(e, t) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid argument");
      RejectTemporalLikeObject(e);
      var r = GetOptionsObject(t),
        o = GetSlot(this, p),
        n = CalendarFields(o, ["day", "month", "monthCode", "year"]);
      var a = PrepareTemporalFields(this, n, []);
      return a = CalendarMergeFields(o, a, PrepareTemporalFields(e, n, "partial")), a = PrepareTemporalFields(a, n, []), CalendarDateFromFields(o, a, r);
    }
  }, {
    key: "withCalendar",
    value: function withCalendar(e) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalCalendarSlotValue(e);
      return new PlainDate(GetSlot(this, i), GetSlot(this, s), GetSlot(this, l), t);
    }
  }, {
    key: "add",
    value: function add(e, t) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var r = ToTemporalDuration(e),
        o = GetOptionsObject(t);
      return CalendarDateAdd(GetSlot(this, p), this, r, o);
    }
  }, {
    key: "subtract",
    value: function subtract(e, t) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var r = CreateNegatedTemporalDuration(ToTemporalDuration(e)),
        o = GetOptionsObject(t);
      return CalendarDateAdd(GetSlot(this, p), this, r, o);
    }
  }, {
    key: "until",
    value: function until(e, t) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainDate("until", this, e, t);
    }
  }, {
    key: "since",
    value: function since(e, t) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainDate("since", this, e, t);
    }
  }, {
    key: "equals",
    value: function equals(e) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e);
      for (var _i22 = 0, _arr8 = [i, s, l]; _i22 < _arr8.length; _i22++) {
        var _e54 = _arr8[_i22];
        if (GetSlot(this, _e54) !== GetSlot(t, _e54)) return !1;
      }
      return CalendarEquals(GetSlot(this, p), GetSlot(t, p));
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return TemporalDateToString(this, ToCalendarNameOption(GetOptionsObject(e)));
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return TemporalDateToString(this);
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return new ht(e, t).format(this);
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use compare() or equals() to compare Temporal.PlainDate");
    }
  }, {
    key: "toPlainDateTime",
    value: function toPlainDateTime(e) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var t = GetSlot(this, i),
        r = GetSlot(this, s),
        o = GetSlot(this, l),
        n = GetSlot(this, p);
      if (void 0 === e) return CreateTemporalDateTime(t, r, o, 0, 0, 0, 0, 0, 0, n);
      var a = ToTemporalTime(e);
      return CreateTemporalDateTime(t, r, o, GetSlot(a, d), GetSlot(a, m), GetSlot(a, c), GetSlot(a, h), GetSlot(a, u), GetSlot(a, T), n);
    }
  }, {
    key: "toZonedDateTime",
    value: function toZonedDateTime(e) {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var t, r;
      if (IsObject(e)) {
        if (IsTemporalTimeZone(e)) t = e;else {
          var _o45 = e.timeZone;
          void 0 === _o45 ? t = ToTemporalTimeZoneSlotValue(e) : (t = ToTemporalTimeZoneSlotValue(_o45), r = e.plainTime);
        }
      } else t = ToTemporalTimeZoneSlotValue(e);
      var o = GetSlot(this, i),
        a = GetSlot(this, s),
        f = GetSlot(this, l),
        y = GetSlot(this, p);
      var I = 0,
        S = 0,
        g = 0,
        w = 0,
        D = 0,
        G = 0;
      void 0 !== r && (r = ToTemporalTime(r), I = GetSlot(r, d), S = GetSlot(r, m), g = GetSlot(r, c), w = GetSlot(r, h), D = GetSlot(r, u), G = GetSlot(r, T));
      return CreateTemporalZonedDateTime(GetSlot(GetInstantFor(t, CreateTemporalDateTime(o, a, f, I, S, g, w, D, G, y), "compatible"), n), t, y);
    }
  }, {
    key: "toPlainYearMonth",
    value: function toPlainYearMonth() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var e = GetSlot(this, p);
      return CalendarYearMonthFromFields(e, PrepareTemporalFields(this, CalendarFields(e, ["monthCode", "year"]), []));
    }
  }, {
    key: "toPlainMonthDay",
    value: function toPlainMonthDay() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      var e = GetSlot(this, p);
      return CalendarMonthDayFromFields(e, PrepareTemporalFields(this, CalendarFields(e, ["day", "monthCode"]), []));
    }
  }, {
    key: "getISOFields",
    value: function getISOFields() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return {
        calendar: GetSlot(this, p),
        isoDay: GetSlot(this, l),
        isoMonth: GetSlot(this, s),
        isoYear: GetSlot(this, i)
      };
    }
  }, {
    key: "getCalendar",
    value: function getCalendar() {
      if (!IsTemporalDate(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarObject(GetSlot(this, p));
    }
  }], [{
    key: "from",
    value: function from(e, t) {
      var r = GetOptionsObject(t);
      return IsTemporalDate(e) ? (ToTemporalOverflow(r), CreateTemporalDate(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l), GetSlot(e, p))) : ToTemporalDate(e, r);
    }
  }, {
    key: "compare",
    value: function compare(e, t) {
      var r = ToTemporalDate(e),
        o = ToTemporalDate(t);
      return CompareISODate(GetSlot(r, i), GetSlot(r, s), GetSlot(r, l), GetSlot(o, i), GetSlot(o, s), GetSlot(o, l));
    }
  }]);
  return PlainDate;
}();
MakeIntrinsicClass(PlainDate, "Temporal.PlainDate");
var PlainDateTime = /*#__PURE__*/function () {
  function PlainDateTime(e, t, r) {
    var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var n = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;
    var a = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : 0;
    var i = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : 0;
    var s = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : 0;
    var l = arguments.length > 8 && arguments[8] !== undefined ? arguments[8] : 0;
    var d = arguments.length > 9 && arguments[9] !== undefined ? arguments[9] : "iso8601";
    _classCallCheck(this, PlainDateTime);
    CreateTemporalDateTimeSlots(this, ToIntegerWithTruncation(e), ToIntegerWithTruncation(t), ToIntegerWithTruncation(r), void 0 === o ? 0 : ToIntegerWithTruncation(o), void 0 === n ? 0 : ToIntegerWithTruncation(n), void 0 === a ? 0 : ToIntegerWithTruncation(a), void 0 === i ? 0 : ToIntegerWithTruncation(i), void 0 === s ? 0 : ToIntegerWithTruncation(s), void 0 === l ? 0 : ToIntegerWithTruncation(l), ToTemporalCalendarSlotValue(d));
  }
  _createClass(PlainDateTime, [{
    key: "calendarId",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarIdentifier(GetSlot(this, p));
    }
  }, {
    key: "year",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarYear(GetSlot(this, p), this);
    }
  }, {
    key: "month",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarMonth(GetSlot(this, p), this);
    }
  }, {
    key: "monthCode",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarMonthCode(GetSlot(this, p), this);
    }
  }, {
    key: "day",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDay(GetSlot(this, p), this);
    }
  }, {
    key: "hour",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, d);
    }
  }, {
    key: "minute",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, m);
    }
  }, {
    key: "second",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, c);
    }
  }, {
    key: "millisecond",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, h);
    }
  }, {
    key: "microsecond",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, u);
    }
  }, {
    key: "nanosecond",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, T);
    }
  }, {
    key: "era",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarEra(GetSlot(this, p), this);
    }
  }, {
    key: "eraYear",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarEraYear(GetSlot(this, p), this);
    }
  }, {
    key: "dayOfWeek",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDayOfWeek(GetSlot(this, p), this);
    }
  }, {
    key: "dayOfYear",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDayOfYear(GetSlot(this, p), this);
    }
  }, {
    key: "weekOfYear",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarWeekOfYear(GetSlot(this, p), this);
    }
  }, {
    key: "yearOfWeek",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarYearOfWeek(GetSlot(this, p), this);
    }
  }, {
    key: "daysInWeek",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInWeek(GetSlot(this, p), this);
    }
  }, {
    key: "daysInYear",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInYear(GetSlot(this, p), this);
    }
  }, {
    key: "daysInMonth",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInMonth(GetSlot(this, p), this);
    }
  }, {
    key: "monthsInYear",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarMonthsInYear(GetSlot(this, p), this);
    }
  }, {
    key: "inLeapYear",
    get: function get() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarInLeapYear(GetSlot(this, p), this);
    }
  }, {
    key: "with",
    value: function _with(e, t) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid argument");
      RejectTemporalLikeObject(e);
      var r = GetOptionsObject(t),
        o = GetSlot(this, p),
        n = CalendarFields(o, ["day", "hour", "microsecond", "millisecond", "minute", "month", "monthCode", "nanosecond", "second", "year"]);
      var a = PrepareTemporalFields(this, n, []);
      a = CalendarMergeFields(o, a, PrepareTemporalFields(e, n, "partial")), a = PrepareTemporalFields(a, n, []);
      var _InterpretTemporalDat4 = InterpretTemporalDateTimeFields(o, a, r),
        i = _InterpretTemporalDat4.year,
        s = _InterpretTemporalDat4.month,
        l = _InterpretTemporalDat4.day,
        d = _InterpretTemporalDat4.hour,
        m = _InterpretTemporalDat4.minute,
        c = _InterpretTemporalDat4.second,
        h = _InterpretTemporalDat4.millisecond,
        u = _InterpretTemporalDat4.microsecond,
        T = _InterpretTemporalDat4.nanosecond;
      return CreateTemporalDateTime(i, s, l, d, m, c, h, u, T, o);
    }
  }, {
    key: "withPlainTime",
    value: function withPlainTime(e) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var t = GetSlot(this, i),
        r = GetSlot(this, s),
        o = GetSlot(this, l),
        n = GetSlot(this, p);
      if (void 0 === e) return CreateTemporalDateTime(t, r, o, 0, 0, 0, 0, 0, 0, n);
      var a = ToTemporalTime(e);
      return CreateTemporalDateTime(t, r, o, GetSlot(a, d), GetSlot(a, m), GetSlot(a, c), GetSlot(a, h), GetSlot(a, u), GetSlot(a, T), n);
    }
  }, {
    key: "withPlainDate",
    value: function withPlainDate(e) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e),
        r = GetSlot(t, i),
        o = GetSlot(t, s),
        n = GetSlot(t, l);
      var a = GetSlot(t, p);
      var f = GetSlot(this, d),
        y = GetSlot(this, m),
        I = GetSlot(this, c),
        S = GetSlot(this, h),
        g = GetSlot(this, u),
        w = GetSlot(this, T);
      return a = ConsolidateCalendars(GetSlot(this, p), a), CreateTemporalDateTime(r, o, n, f, y, I, S, g, w, a);
    }
  }, {
    key: "withCalendar",
    value: function withCalendar(e) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalCalendarSlotValue(e);
      return new PlainDateTime(GetSlot(this, i), GetSlot(this, s), GetSlot(this, l), GetSlot(this, d), GetSlot(this, m), GetSlot(this, c), GetSlot(this, h), GetSlot(this, u), GetSlot(this, T), t);
    }
  }, {
    key: "add",
    value: function add(e, t) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromPlainDateTime("add", this, e, t);
    }
  }, {
    key: "subtract",
    value: function subtract(e, t) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromPlainDateTime("subtract", this, e, t);
    }
  }, {
    key: "until",
    value: function until(e, t) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainDateTime("until", this, e, t);
    }
  }, {
    key: "since",
    value: function since(e, t) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainDateTime("since", this, e, t);
    }
  }, {
    key: "round",
    value: function round(e) {
      var _RoundISODateTime2;
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      if (void 0 === e) throw new TypeError("options parameter is required");
      var t = "string" == typeof e ? CreateOnePropObject("smallestUnit", e) : GetOptionsObject(e),
        r = ToTemporalRoundingIncrement(t),
        o = ToTemporalRoundingMode(t, "halfExpand"),
        n = GetTemporalUnit(t, "smallestUnit", "time", He, ["day"]),
        a = {
          day: 1,
          hour: 24,
          minute: 60,
          second: 60,
          millisecond: 1e3,
          microsecond: 1e3,
          nanosecond: 1e3
        }[n];
      ValidateTemporalRoundingIncrement(r, a, 1 === a);
      var f = GetSlot(this, i),
        y = GetSlot(this, s),
        I = GetSlot(this, l),
        S = GetSlot(this, d),
        g = GetSlot(this, m),
        w = GetSlot(this, c),
        D = GetSlot(this, h),
        G = GetSlot(this, u),
        v = GetSlot(this, T);
      return (_RoundISODateTime2 = RoundISODateTime(f, y, I, S, g, w, D, G, v, r, n, o), f = _RoundISODateTime2.year, y = _RoundISODateTime2.month, I = _RoundISODateTime2.day, S = _RoundISODateTime2.hour, g = _RoundISODateTime2.minute, w = _RoundISODateTime2.second, D = _RoundISODateTime2.millisecond, G = _RoundISODateTime2.microsecond, v = _RoundISODateTime2.nanosecond), CreateTemporalDateTime(f, y, I, S, g, w, D, G, v, GetSlot(this, p));
    }
  }, {
    key: "equals",
    value: function equals(e) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDateTime(e);
      for (var _i23 = 0, _arr9 = [i, s, l, d, m, c, h, u, T]; _i23 < _arr9.length; _i23++) {
        var _e55 = _arr9[_i23];
        if (GetSlot(this, _e55) !== GetSlot(t, _e55)) return !1;
      }
      return CalendarEquals(GetSlot(this, p), GetSlot(t, p));
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var t = GetOptionsObject(e),
        r = ToCalendarNameOption(t),
        o = ToFractionalSecondDigits(t),
        n = ToTemporalRoundingMode(t, "trunc"),
        a = GetTemporalUnit(t, "smallestUnit", "time", void 0);
      if ("hour" === a) throw new RangeError('smallestUnit must be a time unit other than "hour"');
      var _ToSecondsStringPreci2 = ToSecondsStringPrecisionRecord(a, o),
        i = _ToSecondsStringPreci2.precision,
        s = _ToSecondsStringPreci2.unit,
        l = _ToSecondsStringPreci2.increment;
      return TemporalDateTimeToString(this, i, r, {
        unit: s,
        increment: l,
        roundingMode: n
      });
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return TemporalDateTimeToString(this, "auto");
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return new ht(e, t).format(this);
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use compare() or equals() to compare Temporal.PlainDateTime");
    }
  }, {
    key: "toZonedDateTime",
    value: function toZonedDateTime(e, t) {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var r = ToTemporalTimeZoneSlotValue(e);
      return CreateTemporalZonedDateTime(GetSlot(GetInstantFor(r, this, ToTemporalDisambiguation(GetOptionsObject(t))), n), r, GetSlot(this, p));
    }
  }, {
    key: "toPlainDate",
    value: function toPlainDate() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return TemporalDateTimeToDate(this);
    }
  }, {
    key: "toPlainYearMonth",
    value: function toPlainYearMonth() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var e = GetSlot(this, p);
      return CalendarYearMonthFromFields(e, PrepareTemporalFields(this, CalendarFields(e, ["monthCode", "year"]), []));
    }
  }, {
    key: "toPlainMonthDay",
    value: function toPlainMonthDay() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      var e = GetSlot(this, p);
      return CalendarMonthDayFromFields(e, PrepareTemporalFields(this, CalendarFields(e, ["day", "monthCode"]), []));
    }
  }, {
    key: "toPlainTime",
    value: function toPlainTime() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return TemporalDateTimeToTime(this);
    }
  }, {
    key: "getISOFields",
    value: function getISOFields() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return {
        calendar: GetSlot(this, p),
        isoDay: GetSlot(this, l),
        isoHour: GetSlot(this, d),
        isoMicrosecond: GetSlot(this, u),
        isoMillisecond: GetSlot(this, h),
        isoMinute: GetSlot(this, m),
        isoMonth: GetSlot(this, s),
        isoNanosecond: GetSlot(this, T),
        isoSecond: GetSlot(this, c),
        isoYear: GetSlot(this, i)
      };
    }
  }, {
    key: "getCalendar",
    value: function getCalendar() {
      if (!IsTemporalDateTime(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarObject(GetSlot(this, p));
    }
  }], [{
    key: "from",
    value: function from(e, t) {
      var r = GetOptionsObject(t);
      return IsTemporalDateTime(e) ? (ToTemporalOverflow(r), CreateTemporalDateTime(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l), GetSlot(e, d), GetSlot(e, m), GetSlot(e, c), GetSlot(e, h), GetSlot(e, u), GetSlot(e, T), GetSlot(e, p))) : ToTemporalDateTime(e, r);
    }
  }, {
    key: "compare",
    value: function compare(e, t) {
      var r = ToTemporalDateTime(e),
        o = ToTemporalDateTime(t);
      for (var _i24 = 0, _arr10 = [i, s, l, d, m, c, h, u, T]; _i24 < _arr10.length; _i24++) {
        var _e56 = _arr10[_i24];
        var _t50 = GetSlot(r, _e56),
          _n27 = GetSlot(o, _e56);
        if (_t50 !== _n27) return ComparisonResult(_t50 - _n27);
      }
      return 0;
    }
  }]);
  return PlainDateTime;
}();
MakeIntrinsicClass(PlainDateTime, "Temporal.PlainDateTime");
var Duration = /*#__PURE__*/function () {
  function Duration() {
    var e = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
    var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
    var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var n = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;
    var a = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : 0;
    var i = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : 0;
    var s = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : 0;
    var l = arguments.length > 8 && arguments[8] !== undefined ? arguments[8] : 0;
    var d = arguments.length > 9 && arguments[9] !== undefined ? arguments[9] : 0;
    _classCallCheck(this, Duration);
    var m = void 0 === e ? 0 : ToIntegerIfIntegral(e),
      c = void 0 === t ? 0 : ToIntegerIfIntegral(t),
      h = void 0 === r ? 0 : ToIntegerIfIntegral(r),
      u = void 0 === o ? 0 : ToIntegerIfIntegral(o),
      T = void 0 === n ? 0 : ToIntegerIfIntegral(n),
      p = void 0 === a ? 0 : ToIntegerIfIntegral(a),
      f = void 0 === i ? 0 : ToIntegerIfIntegral(i),
      y = void 0 === s ? 0 : ToIntegerIfIntegral(s),
      I = void 0 === l ? 0 : ToIntegerIfIntegral(l),
      S = void 0 === d ? 0 : ToIntegerIfIntegral(d);
    RejectDuration(m, c, h, u, T, p, f, y, I, S), N(this), SetSlot(this, w, m), SetSlot(this, D, c), SetSlot(this, G, h), SetSlot(this, v, u), SetSlot(this, C, T), SetSlot(this, O, p), SetSlot(this, b, f), SetSlot(this, E, y), SetSlot(this, M, I), SetSlot(this, R, S);
  }
  _createClass(Duration, [{
    key: "years",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, w);
    }
  }, {
    key: "months",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, D);
    }
  }, {
    key: "weeks",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, G);
    }
  }, {
    key: "days",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, v);
    }
  }, {
    key: "hours",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, C);
    }
  }, {
    key: "minutes",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, O);
    }
  }, {
    key: "seconds",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, b);
    }
  }, {
    key: "milliseconds",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, E);
    }
  }, {
    key: "microseconds",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, M);
    }
  }, {
    key: "nanoseconds",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, R);
    }
  }, {
    key: "sign",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return DurationSign(GetSlot(this, w), GetSlot(this, D), GetSlot(this, G), GetSlot(this, v), GetSlot(this, C), GetSlot(this, O), GetSlot(this, b), GetSlot(this, E), GetSlot(this, M), GetSlot(this, R));
    }
  }, {
    key: "blank",
    get: function get() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return 0 === DurationSign(GetSlot(this, w), GetSlot(this, D), GetSlot(this, G), GetSlot(this, v), GetSlot(this, C), GetSlot(this, O), GetSlot(this, b), GetSlot(this, E), GetSlot(this, M), GetSlot(this, R));
    }
  }, {
    key: "with",
    value: function _with(e) {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      var t = PrepareTemporalFields(e, ["days", "hours", "microseconds", "milliseconds", "minutes", "months", "nanoseconds", "seconds", "weeks", "years"], "partial"),
        _t$years = t.years,
        r = _t$years === void 0 ? GetSlot(this, w) : _t$years,
        _t$months = t.months,
        o = _t$months === void 0 ? GetSlot(this, D) : _t$months,
        _t$weeks = t.weeks,
        n = _t$weeks === void 0 ? GetSlot(this, G) : _t$weeks,
        _t$days = t.days,
        a = _t$days === void 0 ? GetSlot(this, v) : _t$days,
        _t$hours = t.hours,
        i = _t$hours === void 0 ? GetSlot(this, C) : _t$hours,
        _t$minutes = t.minutes,
        s = _t$minutes === void 0 ? GetSlot(this, O) : _t$minutes,
        _t$seconds = t.seconds,
        l = _t$seconds === void 0 ? GetSlot(this, b) : _t$seconds,
        _t$milliseconds = t.milliseconds,
        d = _t$milliseconds === void 0 ? GetSlot(this, E) : _t$milliseconds,
        _t$microseconds = t.microseconds,
        m = _t$microseconds === void 0 ? GetSlot(this, M) : _t$microseconds,
        _t$nanoseconds = t.nanoseconds,
        c = _t$nanoseconds === void 0 ? GetSlot(this, R) : _t$nanoseconds;
      return new Duration(r, o, n, a, i, s, l, d, m, c);
    }
  }, {
    key: "negated",
    value: function negated() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return CreateNegatedTemporalDuration(this);
    }
  }, {
    key: "abs",
    value: function abs() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return new Duration(Math.abs(GetSlot(this, w)), Math.abs(GetSlot(this, D)), Math.abs(GetSlot(this, G)), Math.abs(GetSlot(this, v)), Math.abs(GetSlot(this, C)), Math.abs(GetSlot(this, O)), Math.abs(GetSlot(this, b)), Math.abs(GetSlot(this, E)), Math.abs(GetSlot(this, M)), Math.abs(GetSlot(this, R)));
    }
  }, {
    key: "add",
    value: function add(e, t) {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromDuration("add", this, e, t);
    }
  }, {
    key: "subtract",
    value: function subtract(e, t) {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromDuration("subtract", this, e, t);
    }
  }, {
    key: "round",
    value: function round(t) {
      var _UnbalanceDurationRel, _RoundDuration8, _AdjustRoundedDuratio2, _BalanceDuration11, _BalanceDurationRelat;
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      if (void 0 === t) throw new TypeError("options parameter is required");
      var r = GetSlot(this, w),
        o = GetSlot(this, D),
        n = GetSlot(this, G),
        a = GetSlot(this, v),
        i = GetSlot(this, C),
        s = GetSlot(this, O),
        l = GetSlot(this, b),
        d = GetSlot(this, E),
        m = GetSlot(this, M),
        c = GetSlot(this, R),
        h = DefaultTemporalLargestUnit(r, o, n, a, i, s, l, d, m, c);
      var u = "string" == typeof t ? CreateOnePropObject("smallestUnit", t) : GetOptionsObject(t);
      var T = GetTemporalUnit(u, "largestUnit", "datetime", void 0, ["auto"]),
        f = ToRelativeTemporalObject(u);
      var y = ToTemporalRoundingIncrement(u),
        I = ToTemporalRoundingMode(u, "halfExpand");
      var S = GetTemporalUnit(u, "smallestUnit", "datetime", void 0),
        g = !0;
      S || (g = !1, S = "nanosecond"), h = LargerOfTwoTemporalUnits(h, S);
      var F = !0;
      if (T || (F = !1, T = h), "auto" === T && (T = h), !g && !F) throw new RangeError("at least one of smallestUnit or largestUnit is required");
      if (LargerOfTwoTemporalUnits(T, S) !== T) throw new RangeError("largestUnit ".concat(T, " cannot be smaller than smallestUnit ").concat(S));
      var Y = {
        hour: 24,
        minute: 60,
        second: 60,
        millisecond: 1e3,
        microsecond: 1e3,
        nanosecond: 1e3
      }[S];
      return void 0 !== Y && ValidateTemporalRoundingIncrement(y, Y, !1), (_UnbalanceDurationRel = UnbalanceDurationRelative(r, o, n, a, T, f), r = _UnbalanceDurationRel.years, o = _UnbalanceDurationRel.months, n = _UnbalanceDurationRel.weeks, a = _UnbalanceDurationRel.days), (_RoundDuration8 = RoundDuration(r, o, n, a, i, s, l, d, m, c, y, S, I, f), r = _RoundDuration8.years, o = _RoundDuration8.months, n = _RoundDuration8.weeks, a = _RoundDuration8.days, i = _RoundDuration8.hours, s = _RoundDuration8.minutes, l = _RoundDuration8.seconds, d = _RoundDuration8.milliseconds, m = _RoundDuration8.microseconds, c = _RoundDuration8.nanoseconds), (_AdjustRoundedDuratio2 = AdjustRoundedDurationDays(r, o, n, a, i, s, l, d, m, c, y, S, I, f), r = _AdjustRoundedDuratio2.years, o = _AdjustRoundedDuratio2.months, n = _AdjustRoundedDuratio2.weeks, a = _AdjustRoundedDuratio2.days, i = _AdjustRoundedDuratio2.hours, s = _AdjustRoundedDuratio2.minutes, l = _AdjustRoundedDuratio2.seconds, d = _AdjustRoundedDuratio2.milliseconds, m = _AdjustRoundedDuratio2.microseconds, c = _AdjustRoundedDuratio2.nanoseconds), (_BalanceDuration11 = BalanceDuration(a, i, s, l, d, m, c, T, f), a = _BalanceDuration11.days, i = _BalanceDuration11.hours, s = _BalanceDuration11.minutes, l = _BalanceDuration11.seconds, d = _BalanceDuration11.milliseconds, m = _BalanceDuration11.microseconds, c = _BalanceDuration11.nanoseconds), (_BalanceDurationRelat = function BalanceDurationRelative(t, r, o, n, a, i) {
        var s = GetIntrinsic("%Temporal.Duration%"),
          l = DurationSign(t, r, o, n, 0, 0, 0, 0, 0, 0);
        if (0 === l) return {
          years: t,
          months: r,
          weeks: o,
          days: n
        };
        var d = BigInt(l);
        var m,
          c,
          h = BigInt(t),
          u = BigInt(r),
          T = BigInt(o),
          f = BigInt(n);
        i && (c = ToTemporalDate(i), m = GetSlot(c, p));
        var y = new s(l),
          I = new s(0, l),
          S = new s(0, 0, l);
        switch (a) {
          case "year":
            {
              if (!m) throw new RangeError("a starting point is required for years balancing");
              var _t51 = "string" != typeof m ? GetMethod(m, "dateAdd") : void 0;
              var _r68, _o46, _n28;
              for (_MoveRelativeDate11 = MoveRelativeDate(m, c, y, _t51), _r68 = _MoveRelativeDate11.relativeTo, _o46 = _MoveRelativeDate11.days, _MoveRelativeDate11; abs(f) >= BigInt(ae(_o46));) {
                var _MoveRelativeDate11, _MoveRelativeDate12;
                f = f - BigInt(_o46), h = h + d, c = _r68, (_MoveRelativeDate12 = MoveRelativeDate(m, c, y, _t51), _r68 = _MoveRelativeDate12.relativeTo, _o46 = _MoveRelativeDate12.days);
              }
              for (_MoveRelativeDate13 = MoveRelativeDate(m, c, I, _t51), _r68 = _MoveRelativeDate13.relativeTo, _n28 = _MoveRelativeDate13.days, _MoveRelativeDate13; abs(f) >= BigInt(ae(_n28));) {
                var _MoveRelativeDate13, _MoveRelativeDate14;
                f = f - BigInt(_n28), u = u + d, c = _r68, (_MoveRelativeDate14 = MoveRelativeDate(m, c, I, _t51), _r68 = _MoveRelativeDate14.relativeTo, _n28 = _MoveRelativeDate14.days);
              }
              _r68 = CalendarDateAdd(m, c, y, void 0, _t51);
              var _a20 = "string" != typeof m ? GetMethod(m, "dateUntil") : void 0,
                _i25 = Te(null);
              _i25.largestUnit = "month";
              var _s8 = CalendarDateUntil(m, c, _r68, _i25, _a20),
                _l9 = GetSlot(_s8, D);
              for (; abs(u) >= BigInt(ae(_l9));) {
                u = u - BigInt(_l9), h = h + d, c = _r68, _r68 = CalendarDateAdd(m, c, y, void 0, _t51);
                var _o47 = Te(null);
                _o47.largestUnit = "month", _s8 = CalendarDateUntil(m, c, _r68, _o47, _a20), _l9 = GetSlot(_s8, D);
              }
              break;
            }
          case "month":
            {
              if (!m) throw new RangeError("a starting point is required for months balancing");
              var _t52 = "string" != typeof m ? GetMethod(m, "dateAdd") : void 0;
              var _r69, _o48;
              for (_MoveRelativeDate15 = MoveRelativeDate(m, c, I, _t52), _r69 = _MoveRelativeDate15.relativeTo, _o48 = _MoveRelativeDate15.days, _MoveRelativeDate15; abs(f) >= BigInt(ae(_o48));) {
                var _MoveRelativeDate15, _MoveRelativeDate16;
                f = f - BigInt(_o48), u = u + d, c = _r69, (_MoveRelativeDate16 = MoveRelativeDate(m, c, I, _t52), _r69 = _MoveRelativeDate16.relativeTo, _o48 = _MoveRelativeDate16.days);
              }
              break;
            }
          case "week":
            {
              if (!m) throw new RangeError("a starting point is required for weeks balancing");
              var _t53 = "string" != typeof m ? GetMethod(m, "dateAdd") : void 0;
              var _r70, _o49;
              for (_MoveRelativeDate17 = MoveRelativeDate(m, c, S, _t53), _r70 = _MoveRelativeDate17.relativeTo, _o49 = _MoveRelativeDate17.days, _MoveRelativeDate17; abs(f) >= BigInt(ae(_o49));) {
                var _MoveRelativeDate17, _MoveRelativeDate18;
                f = f - BigInt(_o49), T = T + d, c = _r70, (_MoveRelativeDate18 = MoveRelativeDate(m, c, S, _t53), _r70 = _MoveRelativeDate18.relativeTo, _o49 = _MoveRelativeDate18.days);
              }
              break;
            }
        }
        return {
          years: Number(h),
          months: Number(u),
          weeks: Number(T),
          days: Number(f)
        };
      }(r, o, n, a, T, f), r = _BalanceDurationRelat.years, o = _BalanceDurationRelat.months, n = _BalanceDurationRelat.weeks, a = _BalanceDurationRelat.days), new Duration(r, o, n, a, i, s, l, d, m, c);
    }
  }, {
    key: "total",
    value: function total(e) {
      var _UnbalanceDurationRel2;
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      var t = GetSlot(this, w),
        r = GetSlot(this, D),
        o = GetSlot(this, G),
        n = GetSlot(this, v),
        a = GetSlot(this, C),
        i = GetSlot(this, O),
        s = GetSlot(this, b),
        l = GetSlot(this, E),
        d = GetSlot(this, M),
        m = GetSlot(this, R);
      if (void 0 === e) throw new TypeError("options argument is required");
      var c = "string" == typeof e ? CreateOnePropObject("unit", e) : GetOptionsObject(e),
        h = ToRelativeTemporalObject(c),
        u = GetTemporalUnit(c, "unit", "datetime", He);
      var T;
      (_UnbalanceDurationRel2 = UnbalanceDurationRelative(t, r, o, n, u, h), t = _UnbalanceDurationRel2.years, r = _UnbalanceDurationRel2.months, o = _UnbalanceDurationRel2.weeks, n = _UnbalanceDurationRel2.days), IsTemporalZonedDateTime(h) && (T = MoveRelativeZonedDateTime(h, t, r, o, 0));
      var p = BalancePossiblyInfiniteDuration(n, a, i, s, l, d, m, u, T);
      if ("positive overflow" === p) return 1 / 0;
      if ("negative overflow" === p) return -1 / 0;
      n = p.days;
      a = p.hours;
      i = p.minutes;
      s = p.seconds;
      l = p.milliseconds;
      d = p.microseconds;
      m = p.nanoseconds;
      var _RoundDuration9 = RoundDuration(t, r, o, n, a, i, s, l, d, m, 1, u, "trunc", h),
        f = _RoundDuration9.total;
      return f;
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      var t = GetOptionsObject(e),
        r = ToFractionalSecondDigits(t),
        o = ToTemporalRoundingMode(t, "trunc"),
        n = GetTemporalUnit(t, "smallestUnit", "time", void 0);
      if ("hour" === n || "minute" === n) throw new RangeError('smallestUnit must be a time unit other than "hours" or "minutes"');
      var _ToSecondsStringPreci3 = ToSecondsStringPrecisionRecord(n, r),
        a = _ToSecondsStringPreci3.precision,
        i = _ToSecondsStringPreci3.unit,
        s = _ToSecondsStringPreci3.increment;
      return TemporalDurationToString(this, a, {
        unit: i,
        increment: s,
        roundingMode: o
      });
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return TemporalDurationToString(this);
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalDuration(this)) throw new TypeError("invalid receiver");
      return "undefined" != typeof Intl && void 0 !== Intl.DurationFormat ? new Intl.DurationFormat(e, t).format(this) : (console.warn("Temporal.Duration.prototype.toLocaleString() requires Intl.DurationFormat."), TemporalDurationToString(this));
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use compare() to compare Temporal.Duration");
    }
  }], [{
    key: "from",
    value: function from(e) {
      return IsTemporalDuration(e) ? new Duration(GetSlot(e, w), GetSlot(e, D), GetSlot(e, G), GetSlot(e, v), GetSlot(e, C), GetSlot(e, O), GetSlot(e, b), GetSlot(e, E), GetSlot(e, M), GetSlot(e, R)) : ToTemporalDuration(e);
    }
  }, {
    key: "compare",
    value: function compare(t, r, o) {
      var _UnbalanceDurationRel3, _UnbalanceDurationRel4;
      var n = ToTemporalDuration(t),
        a = ToTemporalDuration(r),
        i = ToRelativeTemporalObject(GetOptionsObject(o)),
        s = GetSlot(n, w),
        l = GetSlot(n, D),
        d = GetSlot(n, G);
      var m = GetSlot(n, v);
      var c = GetSlot(n, C),
        h = GetSlot(n, O),
        u = GetSlot(n, b),
        T = GetSlot(n, E),
        p = GetSlot(n, M);
      var f = GetSlot(n, R);
      var y = GetSlot(a, w),
        I = GetSlot(a, D),
        S = GetSlot(a, G);
      var g = GetSlot(a, v);
      var F = GetSlot(a, C),
        Y = GetSlot(a, O),
        P = GetSlot(a, b),
        Z = GetSlot(a, E),
        B = GetSlot(a, M);
      var N = GetSlot(a, R);
      var j = CalculateOffsetShift(i, s, l, d, m),
        $ = CalculateOffsetShift(i, y, I, S, g);
      0 === s && 0 === y && 0 === l && 0 === I && 0 === d && 0 === S || ((_UnbalanceDurationRel3 = UnbalanceDurationRelative(s, l, d, m, "day", i), m = _UnbalanceDurationRel3.days), (_UnbalanceDurationRel4 = UnbalanceDurationRelative(y, I, S, g, "day", i), g = _UnbalanceDurationRel4.days));
      var k = TotalDurationNanoseconds(m, c, h, u, T, p, f, j),
        U = TotalDurationNanoseconds(g, F, Y, P, Z, B, N, $);
      return ComparisonResult(Number(k - U));
    }
  }]);
  return Duration;
}();
MakeIntrinsicClass(Duration, "Temporal.Duration");
var bt = Object.create;
var PlainMonthDay = /*#__PURE__*/function () {
  function PlainMonthDay(e, t) {
    var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "iso8601";
    var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1972;
    _classCallCheck(this, PlainMonthDay);
    CreateTemporalMonthDaySlots(this, ToIntegerWithTruncation(e), ToIntegerWithTruncation(t), ToTemporalCalendarSlotValue(r), ToIntegerWithTruncation(o));
  }
  _createClass(PlainMonthDay, [{
    key: "monthCode",
    get: function get() {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return CalendarMonthCode(GetSlot(this, p), this);
    }
  }, {
    key: "day",
    get: function get() {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return CalendarDay(GetSlot(this, p), this);
    }
  }, {
    key: "calendarId",
    get: function get() {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarIdentifier(GetSlot(this, p));
    }
  }, {
    key: "with",
    value: function _with(e, t) {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid argument");
      RejectTemporalLikeObject(e);
      var r = GetOptionsObject(t),
        o = GetSlot(this, p),
        n = CalendarFields(o, ["day", "month", "monthCode", "year"]);
      var a = PrepareTemporalFields(this, n, []);
      return a = CalendarMergeFields(o, a, PrepareTemporalFields(e, n, "partial")), a = PrepareTemporalFields(a, n, []), CalendarMonthDayFromFields(o, a, r);
    }
  }, {
    key: "equals",
    value: function equals(e) {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalMonthDay(e);
      for (var _i26 = 0, _arr11 = [s, l, i]; _i26 < _arr11.length; _i26++) {
        var _e57 = _arr11[_i26];
        if (GetSlot(this, _e57) !== GetSlot(t, _e57)) return !1;
      }
      return CalendarEquals(GetSlot(this, p), GetSlot(t, p));
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return TemporalMonthDayToString(this, ToCalendarNameOption(GetOptionsObject(e)));
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return TemporalMonthDayToString(this);
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return new ht(e, t).format(this);
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use equals() to compare Temporal.PlainMonthDay");
    }
  }, {
    key: "toPlainDate",
    value: function toPlainDate(e) {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("argument should be an object");
      var t = GetSlot(this, p),
        r = CalendarFields(t, ["day", "monthCode"]),
        o = PrepareTemporalFields(this, r, []),
        n = CalendarFields(t, ["year"]);
      var a = CalendarMergeFields(t, o, PrepareTemporalFields(e, n, []));
      a = PrepareTemporalFields(a, _toConsumableArray(new Set([].concat(_toConsumableArray(r), _toConsumableArray(n)))), []);
      var i = bt(null);
      return i.overflow = "reject", CalendarDateFromFields(t, a, i);
    }
  }, {
    key: "getISOFields",
    value: function getISOFields() {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return {
        calendar: GetSlot(this, p),
        isoDay: GetSlot(this, l),
        isoMonth: GetSlot(this, s),
        isoYear: GetSlot(this, i)
      };
    }
  }, {
    key: "getCalendar",
    value: function getCalendar() {
      if (!IsTemporalMonthDay(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarObject(GetSlot(this, p));
    }
  }], [{
    key: "from",
    value: function from(e, t) {
      var r = GetOptionsObject(t);
      return IsTemporalMonthDay(e) ? (ToTemporalOverflow(r), CreateTemporalMonthDay(GetSlot(e, s), GetSlot(e, l), GetSlot(e, p), GetSlot(e, i))) : ToTemporalMonthDay(e, r);
    }
  }]);
  return PlainMonthDay;
}();
MakeIntrinsicClass(PlainMonthDay, "Temporal.PlainMonthDay");
var instant = function instant() {
    return new (GetIntrinsic("%Temporal.Instant%"))(Ve());
  },
  plainDateTime = function plainDateTime(e) {
    var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : DefaultTimeZone();
    var r = ToTemporalTimeZoneSlotValue(t),
      o = ToTemporalCalendarSlotValue(e);
    return GetPlainDateTimeFor(r, instant(), o);
  },
  plainDateTimeISO = function plainDateTimeISO() {
    var e = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : DefaultTimeZone();
    return GetPlainDateTimeFor(ToTemporalTimeZoneSlotValue(e), instant(), "iso8601");
  },
  zonedDateTime = function zonedDateTime(e) {
    var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : DefaultTimeZone();
    var r = ToTemporalTimeZoneSlotValue(t),
      o = ToTemporalCalendarSlotValue(e);
    return CreateTemporalZonedDateTime(Ve(), r, o);
  },
  Et = _defineProperty({
    instant: instant,
    plainDateTime: plainDateTime,
    plainDateTimeISO: plainDateTimeISO,
    plainDate: function plainDate(e) {
      var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : DefaultTimeZone();
      return TemporalDateTimeToDate(plainDateTime(e, t));
    },
    plainDateISO: function plainDateISO() {
      var e = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : DefaultTimeZone();
      return TemporalDateTimeToDate(plainDateTimeISO(e));
    },
    plainTimeISO: function plainTimeISO() {
      var e = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : DefaultTimeZone();
      return TemporalDateTimeToTime(plainDateTimeISO(e));
    },
    timeZoneId: function timeZoneId() {
      return DefaultTimeZone();
    },
    zonedDateTime: zonedDateTime,
    zonedDateTimeISO: function zonedDateTimeISO() {
      var e = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : DefaultTimeZone();
      return zonedDateTime("iso8601", e);
    }
  }, Symbol.toStringTag, "Temporal.Now");
Object.defineProperty(Et, Symbol.toStringTag, {
  value: "Temporal.Now",
  writable: !1,
  enumerable: !1,
  configurable: !0
});
var Mt = Object.assign;
function TemporalTimeToString(e, t, r) {
  var o = GetSlot(e, d),
    n = GetSlot(e, m),
    a = GetSlot(e, c),
    i = GetSlot(e, h),
    s = GetSlot(e, u),
    l = GetSlot(e, T);
  if (r) {
    var _e58 = r.unit,
      _t54 = r.increment,
      _d12 = r.roundingMode;
    var _RoundTime2 = RoundTime(o, n, a, i, s, l, _t54, _e58, _d12);
    o = _RoundTime2.hour;
    n = _RoundTime2.minute;
    a = _RoundTime2.second;
    i = _RoundTime2.millisecond;
    s = _RoundTime2.microsecond;
    l = _RoundTime2.nanosecond;
  }
  return "".concat(ISODateTimePartString(o), ":").concat(ISODateTimePartString(n)).concat(FormatSecondsStringPart(a, i, s, l, t));
}
var PlainTime = /*#__PURE__*/function () {
  function PlainTime() {
    var e = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
    var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
    var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var n = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;
    var a = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : 0;
    _classCallCheck(this, PlainTime);
    var i = void 0 === e ? 0 : ToIntegerWithTruncation(e),
      s = void 0 === t ? 0 : ToIntegerWithTruncation(t),
      l = void 0 === r ? 0 : ToIntegerWithTruncation(r),
      p = void 0 === o ? 0 : ToIntegerWithTruncation(o),
      f = void 0 === n ? 0 : ToIntegerWithTruncation(n),
      y = void 0 === a ? 0 : ToIntegerWithTruncation(a);
    RejectTime(i, s, l, p, f, y), N(this), SetSlot(this, d, i), SetSlot(this, m, s), SetSlot(this, c, l), SetSlot(this, h, p), SetSlot(this, u, f), SetSlot(this, T, y);
  }
  _createClass(PlainTime, [{
    key: "hour",
    get: function get() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, d);
    }
  }, {
    key: "minute",
    get: function get() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, m);
    }
  }, {
    key: "second",
    get: function get() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, c);
    }
  }, {
    key: "millisecond",
    get: function get() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, h);
    }
  }, {
    key: "microsecond",
    get: function get() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, u);
    }
  }, {
    key: "nanosecond",
    get: function get() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, T);
    }
  }, {
    key: "with",
    value: function _with(e, t) {
      var _RegulateTime4;
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid argument");
      RejectTemporalLikeObject(e);
      var r = ToTemporalOverflow(GetOptionsObject(t)),
        o = ToTemporalTimeRecord(e, "partial"),
        n = ToTemporalTimeRecord(this);
      var _Mt = Mt(n, o),
        a = _Mt.hour,
        i = _Mt.minute,
        s = _Mt.second,
        l = _Mt.millisecond,
        d = _Mt.microsecond,
        m = _Mt.nanosecond;
      return (_RegulateTime4 = RegulateTime(a, i, s, l, d, m, r), a = _RegulateTime4.hour, i = _RegulateTime4.minute, s = _RegulateTime4.second, l = _RegulateTime4.millisecond, d = _RegulateTime4.microsecond, m = _RegulateTime4.nanosecond), new PlainTime(a, i, s, l, d, m);
    }
  }, {
    key: "add",
    value: function add(e) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromPlainTime("add", this, e);
    }
  }, {
    key: "subtract",
    value: function subtract(e) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromPlainTime("subtract", this, e);
    }
  }, {
    key: "until",
    value: function until(e, t) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainTime("until", this, e, t);
    }
  }, {
    key: "since",
    value: function since(e, t) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainTime("since", this, e, t);
    }
  }, {
    key: "round",
    value: function round(e) {
      var _RoundTime3;
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      if (void 0 === e) throw new TypeError("options parameter is required");
      var t = "string" == typeof e ? CreateOnePropObject("smallestUnit", e) : GetOptionsObject(e),
        r = ToTemporalRoundingIncrement(t),
        o = ToTemporalRoundingMode(t, "halfExpand"),
        n = GetTemporalUnit(t, "smallestUnit", "time", He);
      ValidateTemporalRoundingIncrement(r, {
        hour: 24,
        minute: 60,
        second: 60,
        millisecond: 1e3,
        microsecond: 1e3,
        nanosecond: 1e3
      }[n], !1);
      var a = GetSlot(this, d),
        i = GetSlot(this, m),
        s = GetSlot(this, c),
        l = GetSlot(this, h),
        p = GetSlot(this, u),
        f = GetSlot(this, T);
      return (_RoundTime3 = RoundTime(a, i, s, l, p, f, r, n, o), a = _RoundTime3.hour, i = _RoundTime3.minute, s = _RoundTime3.second, l = _RoundTime3.millisecond, p = _RoundTime3.microsecond, f = _RoundTime3.nanosecond), new PlainTime(a, i, s, l, p, f);
    }
  }, {
    key: "equals",
    value: function equals(e) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalTime(e);
      for (var _i27 = 0, _arr12 = [d, m, c, h, u, T]; _i27 < _arr12.length; _i27++) {
        var _e59 = _arr12[_i27];
        if (GetSlot(this, _e59) !== GetSlot(t, _e59)) return !1;
      }
      return !0;
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      var t = GetOptionsObject(e),
        r = ToFractionalSecondDigits(t),
        o = ToTemporalRoundingMode(t, "trunc"),
        n = GetTemporalUnit(t, "smallestUnit", "time", void 0);
      if ("hour" === n) throw new RangeError('smallestUnit must be a time unit other than "hour"');
      var _ToSecondsStringPreci4 = ToSecondsStringPrecisionRecord(n, r),
        a = _ToSecondsStringPreci4.precision,
        i = _ToSecondsStringPreci4.unit,
        s = _ToSecondsStringPreci4.increment;
      return TemporalTimeToString(this, a, {
        unit: i,
        increment: s,
        roundingMode: o
      });
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return TemporalTimeToString(this, "auto");
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return new ht(e, t).format(this);
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use compare() or equals() to compare Temporal.PlainTime");
    }
  }, {
    key: "toPlainDateTime",
    value: function toPlainDateTime(e) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e),
        r = GetSlot(t, i),
        o = GetSlot(t, s),
        n = GetSlot(t, l),
        a = GetSlot(t, p);
      return CreateTemporalDateTime(r, o, n, GetSlot(this, d), GetSlot(this, m), GetSlot(this, c), GetSlot(this, h), GetSlot(this, u), GetSlot(this, T), a);
    }
  }, {
    key: "toZonedDateTime",
    value: function toZonedDateTime(e) {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid argument");
      var t = e.plainDate;
      if (void 0 === t) throw new TypeError("missing date property");
      var r = ToTemporalDate(t),
        o = e.timeZone;
      if (void 0 === o) throw new TypeError("missing timeZone property");
      var a = ToTemporalTimeZoneSlotValue(o),
        f = GetSlot(r, i),
        y = GetSlot(r, s),
        I = GetSlot(r, l),
        S = GetSlot(r, p),
        g = GetSlot(this, d),
        w = GetSlot(this, m),
        D = GetSlot(this, c),
        G = GetSlot(this, h),
        v = GetSlot(this, u),
        C = GetSlot(this, T);
      return CreateTemporalZonedDateTime(GetSlot(GetInstantFor(a, new (GetIntrinsic("%Temporal.PlainDateTime%"))(f, y, I, g, w, D, G, v, C, S), "compatible"), n), a, S);
    }
  }, {
    key: "getISOFields",
    value: function getISOFields() {
      if (!IsTemporalTime(this)) throw new TypeError("invalid receiver");
      return {
        isoHour: GetSlot(this, d),
        isoMicrosecond: GetSlot(this, u),
        isoMillisecond: GetSlot(this, h),
        isoMinute: GetSlot(this, m),
        isoNanosecond: GetSlot(this, T),
        isoSecond: GetSlot(this, c)
      };
    }
  }], [{
    key: "from",
    value: function from(e, t) {
      var r = ToTemporalOverflow(GetOptionsObject(t));
      return IsTemporalTime(e) ? new PlainTime(GetSlot(e, d), GetSlot(e, m), GetSlot(e, c), GetSlot(e, h), GetSlot(e, u), GetSlot(e, T)) : ToTemporalTime(e, r);
    }
  }, {
    key: "compare",
    value: function compare(e, t) {
      var r = ToTemporalTime(e),
        o = ToTemporalTime(t);
      for (var _i28 = 0, _arr13 = [d, m, c, h, u, T]; _i28 < _arr13.length; _i28++) {
        var _e60 = _arr13[_i28];
        var _t55 = GetSlot(r, _e60),
          _n29 = GetSlot(o, _e60);
        if (_t55 !== _n29) return ComparisonResult(_t55 - _n29);
      }
      return 0;
    }
  }]);
  return PlainTime;
}();
MakeIntrinsicClass(PlainTime, "Temporal.PlainTime");
var TimeZone = /*#__PURE__*/function () {
  function TimeZone(e) {
    _classCallCheck(this, TimeZone);
    if (arguments.length < 1) throw new RangeError("missing argument: identifier is required");
    var t = GetCanonicalTimeZoneIdentifier(e);
    N(this), SetSlot(this, a, t);
  }
  _createClass(TimeZone, [{
    key: "id",
    get: function get() {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, a);
    }
  }, {
    key: "getOffsetNanosecondsFor",
    value: function getOffsetNanosecondsFor(e) {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalInstant(e),
        r = GetSlot(this, a);
      return IsTimeZoneOffsetString(r) ? ParseTimeZoneOffsetString(r) : GetNamedTimeZoneOffsetNanoseconds(r, GetSlot(t, n));
    }
  }, {
    key: "getOffsetStringFor",
    value: function getOffsetStringFor(e) {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      return GetOffsetStringFor(this, ToTemporalInstant(e));
    }
  }, {
    key: "getPlainDateTimeFor",
    value: function getPlainDateTimeFor(e) {
      var t = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : "iso8601";
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      return GetPlainDateTimeFor(this, ToTemporalInstant(e), ToTemporalCalendarSlotValue(t));
    }
  }, {
    key: "getInstantFor",
    value: function getInstantFor(e, t) {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      return GetInstantFor(this, ToTemporalDateTime(e), ToTemporalDisambiguation(GetOptionsObject(t)));
    }
  }, {
    key: "getPossibleInstantsFor",
    value: function getPossibleInstantsFor(t) {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      var r = ToTemporalDateTime(t),
        o = GetIntrinsic("%Temporal.Instant%"),
        n = GetSlot(this, a);
      if (IsTimeZoneOffsetString(n)) {
        var _t56 = GetUTCEpochNanoseconds(GetSlot(r, i), GetSlot(r, s), GetSlot(r, l), GetSlot(r, d), GetSlot(r, m), GetSlot(r, c), GetSlot(r, h), GetSlot(r, u), GetSlot(r, T));
        if (null === _t56) throw new RangeError("DateTime outside of supported range");
        var _a21 = ParseTimeZoneOffsetString(n);
        return [new o(_t56 - BigInt(_a21))];
      }
      var p = function GetNamedTimeZoneEpochNanoseconds(t, r, o, n, a, i, s, l, d, m) {
        var c = GetUTCEpochNanoseconds(r, o, n, a, i, s, l, d, m);
        if (null === c) throw new RangeError("DateTime outside of supported range");
        var h = c - Ee;
        h < Me && (h = c);
        var u = c + Ee;
        u > Re && (u = c);
        var T = GetNamedTimeZoneOffsetNanoseconds(t, h),
          p = GetNamedTimeZoneOffsetNanoseconds(t, u);
        return (T === p ? [T] : [T, p]).map(function (h) {
          var u = c - BigInt(h),
            T = GetNamedTimeZoneDateTimeParts(t, u);
          if (r === T.year && o === T.month && n === T.day && a === T.hour && i === T.minute && s === T.second && l === T.millisecond && d === T.microsecond && m === T.nanosecond) return u;
        }).filter(function (e) {
          return void 0 !== e;
        });
      }(n, GetSlot(r, i), GetSlot(r, s), GetSlot(r, l), GetSlot(r, d), GetSlot(r, m), GetSlot(r, c), GetSlot(r, h), GetSlot(r, u), GetSlot(r, T));
      return p.map(function (e) {
        return new o(e);
      });
    }
  }, {
    key: "getNextTransition",
    value: function getNextTransition(e) {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalInstant(e),
        r = GetSlot(this, a);
      if (IsTimeZoneOffsetString(r) || "UTC" === r) return null;
      var o = GetSlot(t, n);
      var i = GetIntrinsic("%Temporal.Instant%");
      return o = GetNamedTimeZoneNextTransition(r, o), null === o ? null : new i(o);
    }
  }, {
    key: "getPreviousTransition",
    value: function getPreviousTransition(e) {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalInstant(e),
        r = GetSlot(this, a);
      if (IsTimeZoneOffsetString(r) || "UTC" === r) return null;
      var o = GetSlot(t, n);
      var i = GetIntrinsic("%Temporal.Instant%");
      return o = GetNamedTimeZonePreviousTransition(r, o), null === o ? null : new i(o);
    }
  }, {
    key: "toString",
    value: function toString() {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, a);
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalTimeZone(this)) throw new TypeError("invalid receiver");
      return GetSlot(this, a);
    }
  }], [{
    key: "from",
    value: function from(e) {
      return ToTemporalTimeZoneObject(ToTemporalTimeZoneSlotValue(e));
    }
  }]);
  return TimeZone;
}();
MakeIntrinsicClass(TimeZone, "Temporal.TimeZone"), DefineIntrinsic("Temporal.TimeZone.prototype.getOffsetNanosecondsFor", TimeZone.prototype.getOffsetNanosecondsFor), DefineIntrinsic("Temporal.TimeZone.prototype.getPossibleInstantsFor", TimeZone.prototype.getPossibleInstantsFor);
var Rt = Object.create;
var PlainYearMonth = /*#__PURE__*/function () {
  function PlainYearMonth(e, t) {
    var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "iso8601";
    var o = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 1;
    _classCallCheck(this, PlainYearMonth);
    CreateTemporalYearMonthSlots(this, ToIntegerWithTruncation(e), ToIntegerWithTruncation(t), ToTemporalCalendarSlotValue(r), ToIntegerWithTruncation(o));
  }
  _createClass(PlainYearMonth, [{
    key: "year",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarYear(GetSlot(this, p), this);
    }
  }, {
    key: "month",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarMonth(GetSlot(this, p), this);
    }
  }, {
    key: "monthCode",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarMonthCode(GetSlot(this, p), this);
    }
  }, {
    key: "calendarId",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarIdentifier(GetSlot(this, p));
    }
  }, {
    key: "era",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarEra(GetSlot(this, p), this);
    }
  }, {
    key: "eraYear",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarEraYear(GetSlot(this, p), this);
    }
  }, {
    key: "daysInMonth",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInMonth(GetSlot(this, p), this);
    }
  }, {
    key: "daysInYear",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInYear(GetSlot(this, p), this);
    }
  }, {
    key: "monthsInYear",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarMonthsInYear(GetSlot(this, p), this);
    }
  }, {
    key: "inLeapYear",
    get: function get() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return CalendarInLeapYear(GetSlot(this, p), this);
    }
  }, {
    key: "with",
    value: function _with(e, t) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid argument");
      RejectTemporalLikeObject(e);
      var r = GetOptionsObject(t),
        o = GetSlot(this, p),
        n = CalendarFields(o, ["month", "monthCode", "year"]);
      var a = PrepareTemporalFields(this, n, []);
      return a = CalendarMergeFields(o, a, PrepareTemporalFields(e, n, "partial")), a = PrepareTemporalFields(a, n, []), CalendarYearMonthFromFields(o, a, r);
    }
  }, {
    key: "add",
    value: function add(e, t) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromPlainYearMonth("add", this, e, t);
    }
  }, {
    key: "subtract",
    value: function subtract(e, t) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromPlainYearMonth("subtract", this, e, t);
    }
  }, {
    key: "until",
    value: function until(e, t) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainYearMonth("until", this, e, t);
    }
  }, {
    key: "since",
    value: function since(e, t) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalPlainYearMonth("since", this, e, t);
    }
  }, {
    key: "equals",
    value: function equals(e) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalYearMonth(e);
      for (var _i29 = 0, _arr14 = [i, s, l]; _i29 < _arr14.length; _i29++) {
        var _e61 = _arr14[_i29];
        if (GetSlot(this, _e61) !== GetSlot(t, _e61)) return !1;
      }
      return CalendarEquals(GetSlot(this, p), GetSlot(t, p));
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return TemporalYearMonthToString(this, ToCalendarNameOption(GetOptionsObject(e)));
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return TemporalYearMonthToString(this);
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return new ht(e, t).format(this);
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use compare() or equals() to compare Temporal.PlainYearMonth");
    }
  }, {
    key: "toPlainDate",
    value: function toPlainDate(e) {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("argument should be an object");
      var t = GetSlot(this, p),
        r = CalendarFields(t, ["monthCode", "year"]),
        o = PrepareTemporalFields(this, r, []),
        n = CalendarFields(t, ["day"]);
      var a = CalendarMergeFields(t, o, PrepareTemporalFields(e, n, []));
      a = PrepareTemporalFields(a, _toConsumableArray(new Set([].concat(_toConsumableArray(r), _toConsumableArray(n)))), []);
      var i = Rt(null);
      return i.overflow = "reject", CalendarDateFromFields(t, a, i);
    }
  }, {
    key: "getISOFields",
    value: function getISOFields() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return {
        calendar: GetSlot(this, p),
        isoDay: GetSlot(this, l),
        isoMonth: GetSlot(this, s),
        isoYear: GetSlot(this, i)
      };
    }
  }, {
    key: "getCalendar",
    value: function getCalendar() {
      if (!IsTemporalYearMonth(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarObject(GetSlot(this, p));
    }
  }], [{
    key: "from",
    value: function from(e, t) {
      var r = GetOptionsObject(t);
      return IsTemporalYearMonth(e) ? (ToTemporalOverflow(r), CreateTemporalYearMonth(GetSlot(e, i), GetSlot(e, s), GetSlot(e, p), GetSlot(e, l))) : ToTemporalYearMonth(e, r);
    }
  }, {
    key: "compare",
    value: function compare(e, t) {
      var r = ToTemporalYearMonth(e),
        o = ToTemporalYearMonth(t);
      return CompareISODate(GetSlot(r, i), GetSlot(r, s), GetSlot(r, l), GetSlot(o, i), GetSlot(o, s), GetSlot(o, l));
    }
  }]);
  return PlainYearMonth;
}();
MakeIntrinsicClass(PlainYearMonth, "Temporal.PlainYearMonth");
var Ft = ht.prototype.resolvedOptions,
  Yt = Object.create;
var ZonedDateTime = /*#__PURE__*/function () {
  function ZonedDateTime(e, t) {
    var r = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "iso8601";
    _classCallCheck(this, ZonedDateTime);
    if (arguments.length < 1) throw new TypeError("missing argument: epochNanoseconds is required");
    CreateTemporalZonedDateTimeSlots(this, ToBigInt(e), ToTemporalTimeZoneSlotValue(t), ToTemporalCalendarSlotValue(r));
  }
  _createClass(ZonedDateTime, [{
    key: "calendarId",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarIdentifier(GetSlot(this, p));
    }
  }, {
    key: "timeZoneId",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return ToTemporalTimeZoneIdentifier(GetSlot(this, g));
    }
  }, {
    key: "year",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarYear(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "month",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarMonth(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "monthCode",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarMonthCode(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "day",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDay(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "hour",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(dateTime(this), d);
    }
  }, {
    key: "minute",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(dateTime(this), m);
    }
  }, {
    key: "second",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(dateTime(this), c);
    }
  }, {
    key: "millisecond",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(dateTime(this), h);
    }
  }, {
    key: "microsecond",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(dateTime(this), u);
    }
  }, {
    key: "nanosecond",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetSlot(dateTime(this), T);
    }
  }, {
    key: "era",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarEra(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "eraYear",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarEraYear(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "epochSeconds",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = GetSlot(this, n);
      return Number(BigIntFloorDiv(t, ve));
    }
  }, {
    key: "epochMilliseconds",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = GetSlot(this, n);
      return Number(BigIntFloorDiv(t, Ge));
    }
  }, {
    key: "epochMicroseconds",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return ToBigIntExternal(BigIntFloorDiv(GetSlot(this, n), De));
    }
  }, {
    key: "epochNanoseconds",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return ToBigIntExternal(GetSlot(this, n));
    }
  }, {
    key: "dayOfWeek",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDayOfWeek(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "dayOfYear",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDayOfYear(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "weekOfYear",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarWeekOfYear(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "yearOfWeek",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarYearOfWeek(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "hoursInDay",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = dateTime(this),
        r = GetIntrinsic("%Temporal.PlainDateTime%"),
        o = GetSlot(t, i),
        a = GetSlot(t, s),
        d = GetSlot(t, l),
        m = new r(o, a, d, 0, 0, 0, 0, 0, 0),
        c = AddISODate(o, a, d, 0, 0, 0, 1, "reject"),
        h = new r(c.year, c.month, c.day, 0, 0, 0, 0, 0, 0),
        u = GetSlot(this, g),
        T = GetSlot(GetInstantFor(u, m, "compatible"), n),
        p = GetSlot(GetInstantFor(u, h, "compatible"), n);
      return BigIntDivideToNumber(p - T, Oe);
    }
  }, {
    key: "daysInWeek",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInWeek(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "daysInMonth",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInMonth(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "daysInYear",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarDaysInYear(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "monthsInYear",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarMonthsInYear(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "inLeapYear",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return CalendarInLeapYear(GetSlot(this, p), dateTime(this));
    }
  }, {
    key: "offset",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetOffsetStringFor(GetSlot(this, g), GetSlot(this, S));
    }
  }, {
    key: "offsetNanoseconds",
    get: function get() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return GetOffsetNanosecondsFor(GetSlot(this, g), GetSlot(this, S));
    }
  }, {
    key: "with",
    value: function _with(e, t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      if (!IsObject(e)) throw new TypeError("invalid zoned-date-time-like");
      RejectTemporalLikeObject(e);
      var r = GetOptionsObject(t),
        o = GetSlot(this, p);
      var n = CalendarFields(o, ["day", "hour", "microsecond", "millisecond", "minute", "month", "monthCode", "nanosecond", "second", "year"]);
      n.push("offset");
      var a = PrepareTemporalFields(this, n, ["offset"]);
      a = CalendarMergeFields(o, a, PrepareTemporalFields(e, n, "partial")), a = PrepareTemporalFields(a, n, ["offset"]);
      var i = ToTemporalDisambiguation(r),
        s = ToTemporalOffset(r, "prefer");
      var _InterpretTemporalDat5 = InterpretTemporalDateTimeFields(o, a, r),
        l = _InterpretTemporalDat5.year,
        d = _InterpretTemporalDat5.month,
        m = _InterpretTemporalDat5.day,
        c = _InterpretTemporalDat5.hour,
        h = _InterpretTemporalDat5.minute,
        u = _InterpretTemporalDat5.second,
        T = _InterpretTemporalDat5.millisecond,
        f = _InterpretTemporalDat5.microsecond,
        y = _InterpretTemporalDat5.nanosecond;
      var I = ParseTimeZoneOffsetString(a.offset),
        S = GetSlot(this, g);
      return CreateTemporalZonedDateTime(InterpretISODateTimeOffset(l, d, m, c, h, u, T, f, y, "option", I, S, i, s, !1), S, o);
    }
  }, {
    key: "withPlainDate",
    value: function withPlainDate(e) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalDate(e),
        r = GetSlot(t, i),
        o = GetSlot(t, s),
        a = GetSlot(t, l);
      var f = GetSlot(t, p);
      var y = dateTime(this),
        I = GetSlot(y, d),
        S = GetSlot(y, m),
        w = GetSlot(y, c),
        D = GetSlot(y, h),
        G = GetSlot(y, u),
        v = GetSlot(y, T);
      f = ConsolidateCalendars(GetSlot(this, p), f);
      var C = GetSlot(this, g);
      return CreateTemporalZonedDateTime(GetSlot(GetInstantFor(C, new (GetIntrinsic("%Temporal.PlainDateTime%"))(r, o, a, I, S, w, D, G, v, f), "compatible"), n), C, f);
    }
  }, {
    key: "withPlainTime",
    value: function withPlainTime(e) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = GetIntrinsic("%Temporal.PlainTime%"),
        r = void 0 === e ? new t() : ToTemporalTime(e),
        o = dateTime(this),
        a = GetSlot(o, i),
        f = GetSlot(o, s),
        y = GetSlot(o, l),
        I = GetSlot(this, p),
        S = GetSlot(r, d),
        w = GetSlot(r, m),
        D = GetSlot(r, c),
        G = GetSlot(r, h),
        v = GetSlot(r, u),
        C = GetSlot(r, T),
        O = GetSlot(this, g);
      return CreateTemporalZonedDateTime(GetSlot(GetInstantFor(O, new (GetIntrinsic("%Temporal.PlainDateTime%"))(a, f, y, S, w, D, G, v, C, I), "compatible"), n), O, I);
    }
  }, {
    key: "withTimeZone",
    value: function withTimeZone(e) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalTimeZoneSlotValue(e);
      return CreateTemporalZonedDateTime(GetSlot(this, n), t, GetSlot(this, p));
    }
  }, {
    key: "withCalendar",
    value: function withCalendar(e) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = ToTemporalCalendarSlotValue(e);
      return CreateTemporalZonedDateTime(GetSlot(this, n), GetSlot(this, g), t);
    }
  }, {
    key: "add",
    value: function add(e, t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromZonedDateTime("add", this, e, t);
    }
  }, {
    key: "subtract",
    value: function subtract(e, t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return AddDurationToOrSubtractDurationFromZonedDateTime("subtract", this, e, t);
    }
  }, {
    key: "until",
    value: function until(e, t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalZonedDateTime("until", this, e, t);
    }
  }, {
    key: "since",
    value: function since(e, t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return DifferenceTemporalZonedDateTime("since", this, e, t);
    }
  }, {
    key: "round",
    value: function round(t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      if (void 0 === t) throw new TypeError("options parameter is required");
      var r = "string" == typeof t ? CreateOnePropObject("smallestUnit", t) : GetOptionsObject(t),
        o = ToTemporalRoundingIncrement(r),
        a = ToTemporalRoundingMode(r, "halfExpand"),
        f = GetTemporalUnit(r, "smallestUnit", "time", He, ["day"]),
        y = {
          day: 1,
          hour: 24,
          minute: 60,
          second: 60,
          millisecond: 1e3,
          microsecond: 1e3,
          nanosecond: 1e3
        }[f];
      ValidateTemporalRoundingIncrement(o, y, 1 === y);
      var I = dateTime(this);
      var w = GetSlot(I, i),
        D = GetSlot(I, s),
        G = GetSlot(I, l),
        v = GetSlot(I, d),
        C = GetSlot(I, m),
        O = GetSlot(I, c),
        b = GetSlot(I, h),
        E = GetSlot(I, u),
        M = GetSlot(I, T);
      var R = GetIntrinsic("%Temporal.PlainDateTime%"),
        F = GetSlot(this, g),
        Y = GetSlot(this, p),
        P = GetInstantFor(F, new R(GetSlot(I, i), GetSlot(I, s), GetSlot(I, l), 0, 0, 0, 0, 0, 0), "compatible"),
        Z = AddZonedDateTime(P, F, Y, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
        B = Z - BigInt(GetSlot(P, n));
      if (B <= Ie) throw new RangeError("cannot round a ZonedDateTime in a calendar with zero or negative length days");
      var _RoundISODateTime3 = RoundISODateTime(w, D, G, v, C, O, b, E, M, o, f, a, Number(B));
      w = _RoundISODateTime3.year;
      D = _RoundISODateTime3.month;
      G = _RoundISODateTime3.day;
      v = _RoundISODateTime3.hour;
      C = _RoundISODateTime3.minute;
      O = _RoundISODateTime3.second;
      b = _RoundISODateTime3.millisecond;
      E = _RoundISODateTime3.microsecond;
      M = _RoundISODateTime3.nanosecond;
      return CreateTemporalZonedDateTime(InterpretISODateTimeOffset(w, D, G, v, C, O, b, E, M, "option", GetOffsetNanosecondsFor(F, GetSlot(this, S)), F, "compatible", "prefer", !1), F, GetSlot(this, p));
    }
  }, {
    key: "equals",
    value: function equals(t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var r = ToTemporalZonedDateTime(t),
        o = GetSlot(this, n),
        a = GetSlot(r, n);
      return !!(BigInt(o) === BigInt(a)) && !!TimeZoneEquals(GetSlot(this, g), GetSlot(r, g)) && CalendarEquals(GetSlot(this, p), GetSlot(r, p));
    }
  }, {
    key: "toString",
    value: function toString(e) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var t = GetOptionsObject(e),
        r = ToCalendarNameOption(t),
        o = ToFractionalSecondDigits(t),
        n = function ToShowOffsetOption(e) {
          return GetOption(e, "offset", ["auto", "never"], "auto");
        }(t),
        a = ToTemporalRoundingMode(t, "trunc"),
        i = GetTemporalUnit(t, "smallestUnit", "time", void 0);
      if ("hour" === i) throw new RangeError('smallestUnit must be a time unit other than "hour"');
      var s = function ToTimeZoneNameOption(e) {
          return GetOption(e, "timeZoneName", ["auto", "never", "critical"], "auto");
        }(t),
        _ToSecondsStringPreci5 = ToSecondsStringPrecisionRecord(i, o),
        l = _ToSecondsStringPreci5.precision,
        d = _ToSecondsStringPreci5.unit,
        m = _ToSecondsStringPreci5.increment;
      return TemporalZonedDateTimeToString(this, l, r, s, n, {
        unit: d,
        increment: m,
        roundingMode: a
      });
    }
  }, {
    key: "toLocaleString",
    value: function toLocaleString(e, t) {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var r = GetOptionsObject(t),
        o = Yt(null);
      if (CopyDataProperties(o, r, ["timeZone"]), void 0 !== r.timeZone) throw new TypeError("ZonedDateTime toLocaleString does not accept a timeZone option");
      void 0 === o.year && void 0 === o.month && void 0 === o.day && void 0 === o.weekday && void 0 === o.dateStyle && void 0 === o.hour && void 0 === o.minute && void 0 === o.second && void 0 === o.timeStyle && void 0 === o.dayPeriod && void 0 === o.timeZoneName && (o.timeZoneName = "short");
      var n = ToTemporalTimeZoneIdentifier(GetSlot(this, g));
      if (IsTimeZoneOffsetString(n)) throw new RangeError("toLocaleString does not support offset string time zones");
      n = GetCanonicalTimeZoneIdentifier(n), o.timeZone = n;
      var a = new ht(e, o),
        i = Call(Ft, a, []).calendar,
        s = ToTemporalCalendarIdentifier(GetSlot(this, p));
      if ("iso8601" !== s && "iso8601" !== i && i !== s) throw new RangeError("cannot format ZonedDateTime with calendar ".concat(s, " in locale with calendar ").concat(i));
      return a.format(GetSlot(this, S));
    }
  }, {
    key: "toJSON",
    value: function toJSON() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return TemporalZonedDateTimeToString(this, "auto");
    }
  }, {
    key: "valueOf",
    value: function valueOf() {
      throw new TypeError("use compare() or equals() to compare Temporal.ZonedDateTime");
    }
  }, {
    key: "startOfDay",
    value: function startOfDay() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var e = dateTime(this),
        t = GetIntrinsic("%Temporal.PlainDateTime%"),
        r = GetSlot(this, p),
        o = new t(GetSlot(e, i), GetSlot(e, s), GetSlot(e, l), 0, 0, 0, 0, 0, 0, r),
        a = GetSlot(this, g);
      return CreateTemporalZonedDateTime(GetSlot(GetInstantFor(a, o, "compatible"), n), a, r);
    }
  }, {
    key: "toInstant",
    value: function toInstant() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return new (GetIntrinsic("%Temporal.Instant%"))(GetSlot(this, n));
    }
  }, {
    key: "toPlainDate",
    value: function toPlainDate() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return TemporalDateTimeToDate(dateTime(this));
    }
  }, {
    key: "toPlainTime",
    value: function toPlainTime() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return TemporalDateTimeToTime(dateTime(this));
    }
  }, {
    key: "toPlainDateTime",
    value: function toPlainDateTime() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return dateTime(this);
    }
  }, {
    key: "toPlainYearMonth",
    value: function toPlainYearMonth() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var e = GetSlot(this, p);
      return CalendarYearMonthFromFields(e, PrepareTemporalFields(this, CalendarFields(e, ["monthCode", "year"]), []));
    }
  }, {
    key: "toPlainMonthDay",
    value: function toPlainMonthDay() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var e = GetSlot(this, p);
      return CalendarMonthDayFromFields(e, PrepareTemporalFields(this, CalendarFields(e, ["day", "monthCode"]), []));
    }
  }, {
    key: "getISOFields",
    value: function getISOFields() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      var e = dateTime(this),
        t = GetSlot(this, g);
      return {
        calendar: GetSlot(this, p),
        isoDay: GetSlot(e, l),
        isoHour: GetSlot(e, d),
        isoMicrosecond: GetSlot(e, u),
        isoMillisecond: GetSlot(e, h),
        isoMinute: GetSlot(e, m),
        isoMonth: GetSlot(e, s),
        isoNanosecond: GetSlot(e, T),
        isoSecond: GetSlot(e, c),
        isoYear: GetSlot(e, i),
        offset: GetOffsetStringFor(t, GetSlot(this, S)),
        timeZone: t
      };
    }
  }, {
    key: "getCalendar",
    value: function getCalendar() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return ToTemporalCalendarObject(GetSlot(this, p));
    }
  }, {
    key: "getTimeZone",
    value: function getTimeZone() {
      if (!IsTemporalZonedDateTime(this)) throw new TypeError("invalid receiver");
      return ToTemporalTimeZoneObject(GetSlot(this, g));
    }
  }], [{
    key: "from",
    value: function from(e, t) {
      var r = GetOptionsObject(t);
      return IsTemporalZonedDateTime(e) ? (ToTemporalDisambiguation(r), ToTemporalOffset(r, "reject"), ToTemporalOverflow(r), CreateTemporalZonedDateTime(GetSlot(e, n), GetSlot(e, g), GetSlot(e, p))) : ToTemporalZonedDateTime(e, r);
    }
  }, {
    key: "compare",
    value: function compare(t, r) {
      var o = ToTemporalZonedDateTime(t),
        a = ToTemporalZonedDateTime(r),
        i = GetSlot(o, n),
        s = GetSlot(a, n);
      return BigInt(i) < BigInt(s) ? -1 : BigInt(i) > BigInt(s) ? 1 : 0;
    }
  }]);
  return ZonedDateTime;
}();
function dateTime(e) {
  return GetPlainDateTimeFor(GetSlot(e, g), GetSlot(e, S), GetSlot(e, p));
}
MakeIntrinsicClass(ZonedDateTime, "Temporal.ZonedDateTime");
var Pt = Object.freeze({
  __proto__: null,
  Calendar: Calendar,
  Duration: Duration,
  Instant: Instant,
  Now: Et,
  PlainDate: PlainDate,
  PlainDateTime: PlainDateTime,
  PlainMonthDay: PlainMonthDay,
  PlainTime: PlainTime,
  PlainYearMonth: PlainYearMonth,
  TimeZone: TimeZone,
  ZonedDateTime: ZonedDateTime
});
function toTemporalInstant() {
  var t = BigInt(+this) * Ge;
  return new Instant(t);
}
var Zt = [Instant, Calendar, PlainDate, PlainDateTime, Duration, PlainMonthDay, PlainTime, TimeZone, PlainYearMonth, ZonedDateTime];
for (var _i30 = 0, _Zt = Zt; _i30 < _Zt.length; _i30++) {
  var _e62 = _Zt[_i30];
  var _t57 = Object.getOwnPropertyDescriptor(_e62, "prototype");
  (_t57.configurable || _t57.enumerable || _t57.writable) && (_t57.configurable = !1, _t57.enumerable = !1, _t57.writable = !1, Object.defineProperty(_e62, "prototype", _t57));
}

var TemporalPolyfill = /*#__PURE__*/Object.freeze({
  __proto__: null,
  Intl: ut,
  Temporal: Pt,
  toTemporalInstant: toTemporalInstant
});

globalThis.TemporalPolyfill = TemporalPolyfill;
