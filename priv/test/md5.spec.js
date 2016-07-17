import {md5, md5Arr} from '../static/scripts/md5.js';

describe('MD5', function () {
    describe('hexadecimal string', function () {
        it('should be 5d41402abc4b2a76b9719d911017c592 for md5("hello")', function () {
            expect(md5('hello')).toEqual('5d41402abc4b2a76b9719d911017c592');
        });

        it('should work for empty string', function () {
            expect(md5('')).toEqual('d41d8cd98f00b204e9800998ecf8427e');
        });
    });

    describe('array representation', function () {
        it('should be  for md5Arr("hello")', function () {
            expect(md5Arr('hello'))
                .toEqual([93, 65, 64, 42, 188, 75, 42, 118, 185, 113, 157, 145, 16, 23, 197, 146]);
        });

        it('should be defined for md5Arr("")', function () {
            expect(md5Arr(''))
                .toEqual([212, 29, 140, 217, 143, 0, 178, 4, 233, 128, 9, 152, 236, 248, 66, 126]);
        });
    });
});
