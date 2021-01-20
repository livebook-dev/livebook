import Delta from '../../js/lib/delta';

describe('Delta', () => {
  describe('compose', () => {
    it('insert with insert', () => {
      const a = new Delta().insert('A');
      const b = new Delta().insert('B');
      const expected = new Delta().insert('B').insert('A');

      expect(a.compose(b)).toEqual(expected);
    });

    it('insert with retain', () => {
      const a = new Delta().insert('A');
      const b = new Delta().retain(1);
      const expected = new Delta().insert('A');

      expect(a.compose(b)).toEqual(expected);
    });

    it('insert with delete', () => {
      const a = new Delta().insert('A');
      const b = new Delta().delete(1);
      const expected = new Delta();

      expect(a.compose(b)).toEqual(expected);
    });

    it('retain with insert', () => {
      const a = new Delta().retain(1);
      const b = new Delta().insert('B');
      const expected = new Delta().insert('B');

      expect(a.compose(b)).toEqual(expected);
    });

    it('retain with retain', () => {
      const a = new Delta().retain(1);
      const b = new Delta().retain(1);
      const expected = new Delta();

      expect(a.compose(b)).toEqual(expected);
    });

    it('retain with delete', () => {
      const a = new Delta().retain(1);
      const b = new Delta().delete(1);
      const expected = new Delta().delete(1);

      expect(a.compose(b)).toEqual(expected);
    });

    it('delete with insert', () => {
      const a = new Delta().delete(1);
      const b = new Delta().insert('B');
      const expected = new Delta().insert('B').delete(1);

      expect(a.compose(b)).toEqual(expected);
    });

    it('delete with retain', () => {
      const a = new Delta().delete(1);
      const b = new Delta().retain(1);
      const expected = new Delta().delete(1);

      expect(a.compose(b)).toEqual(expected);
    });

    it('delete with delete', () => {
      const a = new Delta().delete(1);
      const b = new Delta().delete(1);
      const expected = new Delta().delete(2);

      expect(a.compose(b)).toEqual(expected);
    });

    it('insert in the middle of a text', () => {
      const a = new Delta().insert('Hello');
      const b = new Delta().retain(3).insert('X');
      const expected = new Delta().insert('HelXlo');

      expect(a.compose(b)).toEqual(expected);
    });

    it('insert and delete with different ordering', () => {
      const a = new Delta().insert('Hello');
      const b = new Delta().insert('Hello');

      const insertFirst = new Delta()
        .retain(3)
        .insert('X')
        .delete(1);

        const deleteFirst = new Delta()
        .retain(3)
        .delete(1)
        .insert('X');

      const expected = new Delta().insert('HelXo');

      expect(a.compose(insertFirst)).toEqual(expected);
      expect(b.compose(deleteFirst)).toEqual(expected);
    });

    it('retain then insert with delete entire text', () => {
      const a = new Delta().retain(4).insert('Hello');
      const b = new Delta().delete(9);
      const expected = new Delta().delete(4);

      expect(a.compose(b)).toEqual(expected);
    });

    it('retain more than the length of text', () => {
      const a = new Delta().insert('Hello');
      const b = new Delta().retain(10);
      const expected = new Delta().insert('Hello');

      expect(a.compose(b)).toEqual(expected);
    });
  });

  describe('transform', () => {
    it('insert against insert', () => {
      const a = new Delta().insert('A');
      const b = new Delta().insert('B');
      const bPrimeAssumingAFirst = new Delta().retain(1).insert('B');
      const bPrimeAssumingBFirst = new Delta().insert('B');
      expect(a.transform(b, "left")).toEqual(bPrimeAssumingAFirst);
      expect(a.transform(b, "right")).toEqual(bPrimeAssumingBFirst);
    });

    it('retain against insert', () => {
      const a = new Delta().insert('A');
      const b = new Delta().retain(1).insert('B');
      const bPrime = new Delta().retain(2).insert('B');
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('delete against insert', () => {
      const a = new Delta().insert('A');
      const b = new Delta().delete(1);
      const bPrime = new Delta().retain(1).delete(1);
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('insert against delete', () => {
      const a = new Delta().delete(1);
      const b = new Delta().insert('B');
      const bPrime = new Delta().insert('B');
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('retain against delete', () => {
      const a = new Delta().delete(1);
      const b = new Delta().retain(1);
      const bPrime = new Delta();
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('delete against delete', () => {
      const a = new Delta().delete(1);
      const b = new Delta().delete(1);
      const bPrime = new Delta();
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('insert against retain', () => {
      const a = new Delta().retain(1).insert('A');
      const b = new Delta().insert('B');
      const bPrime = new Delta().insert('B');
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('retain against retain', () => {
      const a = new Delta().retain(1).insert('A');
      const b = new Delta().retain(1).insert('B');
      const bPrime = new Delta().retain(1).insert('B')
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('delete against retain', () => {
      const a = new Delta().retain(1);
      const b = new Delta().delete(1);
      const bPrime = new Delta().delete(1);
      expect(a.transform(b, "right")).toEqual(bPrime);
    });

    it('alternating edits', () => {
      const a = new Delta()
        .retain(2)
        .insert('si')
        .delete(5);

      const b = new Delta()
        .retain(1)
        .insert('e')
        .delete(5)
        .retain(1)
        .insert('ow');

      const bPrimeAssumingBFirst = new Delta()
        .retain(1)
        .insert('e')
        .delete(1)
        .retain(2)
        .insert('ow');

      const aPrimeAssumingBFirst = new Delta()
        .retain(2)
        .insert('si')
        .delete(1);

      expect(a.transform(b, "right")).toEqual(bPrimeAssumingBFirst);
      expect(b.transform(a, "left")).toEqual(aPrimeAssumingBFirst);
    });

    it('conflicting appends', () => {
      const a = new Delta().retain(3).insert('aa');
      const b = new Delta().retain(3).insert('bb');
      const bPrimeAssumingBFirst = new Delta().retain(3).insert('bb');
      const aPrimeAssumingBFirst = new Delta().retain(5).insert('aa');
      expect(a.transform(b, "right")).toEqual(bPrimeAssumingBFirst);
      expect(b.transform(a, "left")).toEqual(aPrimeAssumingBFirst);
    });

    it('prepend and append', () => {
      const a = new Delta().insert('aa');
      const b = new Delta().retain(3).insert('bb');
      const bPrime = new Delta().retain(5).insert('bb');
      const aPrime = new Delta().insert('aa');
      expect(a.transform(b, "right")).toEqual(bPrime);
      expect(b.transform(a, "left")).toEqual(aPrime);
    });

    it('trailing deletes with differing lengths', () => {
      const a = new Delta().retain(2).delete(1);
      const b = new Delta().delete(3);
      const bPrime = new Delta().delete(2);
      const aPrime = new Delta();
      expect(a.transform(b, "right")).toEqual(bPrime);
      expect(b.transform(a, "left")).toEqual(aPrime);
    });

    it('immutability', () => {
      const a = new Delta().insert('A');
      const b = new Delta().insert('B');
      const bPrime = new Delta().retain(1).insert('B');
      expect(a.transform(b, "left")).toEqual(bPrime);

      expect(a).toEqual(new Delta().insert('A'));
      expect(b).toEqual(new Delta().insert('B'));
    });
  });
});
