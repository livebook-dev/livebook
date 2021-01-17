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
});
