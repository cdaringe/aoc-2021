const fs = require("fs");
const { resolve } = require("path");
const lines = fs
  .readFileSync(resolve(__dirname, "../input"), "utf8")
  .split("\n");
const vent_counts = lines
  .filter(Boolean)
  .map((l) => l.split(" -> ").map((s) => s.split(",").map((i) => parseInt(i))))
  .filter(([[x1, y1], [x2, y2]]) => x1 === x2 || y1 === y2)
  .flatMap(([[x1, y1], [x2, y2]]) => {
    const [dx, dy] = x1 === x2 ? [0, y1 > y2 ? -1 : 1] : [x1 > x2 ? -1 : 1, 0];
    const fill = (points) => {
      const [[xx, yy], ...rest] = points;
      if (xx == x2 && yy == y2) return points;
      const nextPoint = [xx + dx, yy + dy];
      return fill([nextPoint, ...points]);
    };
    return fill([[x1, y1]]);
  })
  .reduce((acc, [x, y]) => {
    const curr = acc[`${x}_${y}`] || 0;
    acc[`${x}_${y}`] = curr + 1;
    return acc;
  }, {});
console.log(Object.values(vent_counts).filter((v) => v > 1).length);
