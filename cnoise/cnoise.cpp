// cnoise.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "cnoise.h"

struct Vec4
{
	float m[4];
	Vec4( float x, float y, float z, float w )
		: m{ x, y, z, w }
	{}
	Vec4( const float* f )
		: m{ f[0], f[1], f[2], f[3] }
	{}
	inline float& operator[](const int idx)
	{
		return m[idx];
	}
	inline float dot(const Vec4& b)
	{
		return m[0] * b.m[0] + m[1] * b.m[1] + m[2] * b.m[2] + m[3] * b.m[3];
	}
};

inline Vec4 operator +(const Vec4& l, const Vec4& r)
{
	Vec4(l.m[0] + r.m[0], l.m[1] + r.m[1], l.m[2] + r.m[2], l.m[3] + r.m[3]);
}

inline Vec4 operator -(const Vec4& l, const Vec4& r)
{
	Vec4(l.m[0] - r.m[0], l.m[1] - r.m[1], l.m[2] - r.m[2], l.m[3] - r.m[3]);
}

inline Vec4 operator *(const Vec4& l, const Vec4& r)
{
	Vec4(l.m[0] * r.m[0], l.m[1] * r.m[1], l.m[2] * r.m[2], l.m[3] * r.m[3]);
}

inline Vec4 operator *(const Vec4& l, const float r )
{
	Vec4(l.m[0] * r, l.m[1] * r, l.m[2] * r, l.m[3] * r);
}

CNOISE_API int CN_Simplex3DDeriv( float x, float y, float z, float* results )
{
	const float kSkeweFactor = 1.0f / 3.0f;
	const float kUnskewFactor = 1.0f / 6.0f;
}
