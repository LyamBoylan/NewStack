from math import *
from random import *

import sys

rcode=""

if '.ns' not in sys.argv[1]:
	print('FileTypeError: File is not a NewStack file type ( .ns )')
	sys.exit()

with open(sys.argv[1], encoding='utf-8') as f:
		for line in f:
			rcode+=line

infinity=10**6
epsilon=1./infinity


stack=[]
Pstack=True

sets=['P','R','Z','N','□','S']
operations='+-^*/%'
booleans="><=≠ϵ≈≤≥:\',"
constants=['j','π','τ','η','∞','ε','½','e','Φ','φ','L','ᵢ','x']	
values=[1j,pi,2*pi,pi/2,infinity,epsilon,.5,e,(1-5**.5)/2,(1+5**.5)/2,0,0,0]
whitespaces=" ]|\n"
functions=['I','i','!','c','ʁ','f','ᴇ','ₑ','√','∂','Σ','∏','∫','Ẽ','ₒ',';','ᴙ','ﬁ']
eops=['^','²','³','⁴','⁺','⁻']
eopequals=['**','**2','**3','**4','+1','-1']

fdc=[[0,4./5,-1./5,4./105,-1./280]
		,[-205./72,8./5,-1./5,8./315,-1./560]
		,[0,-61./30,169./120,-3./10,7./240]]

cfunct='x'

def function(tp, array):
	#print('function debug:',tp,array)
	if tp=='I':return(stack[number(array[0])%len(stack)])
	elif tp=='i':return(stack.index(number(array[0])))
	elif tp=='!':return(factorial(number(array[0])))
	elif tp=='c':return(choose(number(array[0]),number(array[1])))
	elif tp=='ʁ':
		if len(array)==1:return(randint(0,number(array[0])))
		else:return(randint(number(array[0]),number(array[1])))
	elif tp=='f':
		values[-1]=number(array[0])
		return(number(cfunct))
	elif tp=='ᴇ':return(10**number(array[0]))
	elif tp=='ₑ':return(e**number(array[0]))
	elif tp=='√':
		if len(array)==1:return(number(array[0])**.5)
		else:return(number(array[0])**(1./number(array[1])))
	elif tp=='∂':
		degree=1
		if len(array)==1:
			formula=cfunct
			num=number(array[0])
		else:
			if array[0]=='f':
				formula=cfunct
				num=number(array[1])
			else:
				formula=array[0]
				num=number(array[1])
		if len(array)==3:degree=int(number(array[2]))
		return(derivative(formula,num,degree))
	elif tp=='Σ':
		formula=array[0]
		lower=number(array[1])
		higher=number(array[2])
		terms=[]
		for x in range(int(lower),int(higher)+1):
			values[-1]=x
			terms+=[number(formula)]
		return(sum(terms))
	elif tp=='∏':
		formula=array[0]
		lower=number(array[1])
		higher=number(array[2])
		terms=[]
		for x in range(int(lower),int(higher)+1):
			values[-1]=x
			terms+=[number(formula)]
		return(product(terms))
	elif tp=='∫':
		formula=array[0]
		lower=number(array[1])
		higher=number(array[2])

		terms=[]

		M=infinity
		D=(higher-lower)/M
		for j in range(0,int(M/2)):
			values[-1]=lower+2*j*D
			terms+=[number(formula)]
			values[-1]=lower+(2*j+1)*D
			terms+=[4*number(formula)]
			values[-1]=lower+(2*j+2)*D
			terms+=[number(formula)]
	elif tp=='Ẽ':
		if len(array)==1:
			formula=cfunct
			search=array[0]
			ix=1
		elif len(array)==2:
			formula=array[0]
			search=array[1]
			ix=1
		elif len(array)==3:
			formula=array[0]
			search=array[1]
			ix=number(array[2])
		values[-1]=ix
		itr=0
		while abs(number(search)-number(formula))>epsilon:
			ix-=.1*number(formula+'-('+search+')')/derivative(formula,ix)
			values[-1]=ix
			itr+=1
			if itr>infinity**.5:break
		if abs(number(search)-number(formula))>epsilon**.5:
			ix=-1
			values[-1]=ix
			itr=0
			while abs(number(search)-number(formula))>epsilon:
				ix-=.1*number(formula+'-('+search+')')/derivative(formula,ix)
				values[-1]=ix
				itr+=1
				if itr>infinity**.5:break
		if abs(number(search)-number(formula))>epsilon:
			ix=1j+1
			values[-1]=ix
			itr=0
			while abs(number(search)-number(formula))>epsilon:
				ix-=.1*number(formula+'-('+search+')')/derivative(formula+'-('+search+')',ix)
				values[-1]=ix
				itr+=1
				if itr>infinity**.5:break
		values[-1]=ix
		#print('UO:',number(search),number(formula),ix)
		return(rationalize(ix))
	elif tp=='ₒ':
		f=array[0][0]
		num=number(array[0][1:len(array[0])-1])
		itr=number(array[1])
		#print('CHAIN:',f,num,itr)
		for _ in range(int(itr)):
			values[-1]=num
			#print('NUM:',f+str(num)+'}')
			num=number(f+str(num)+'}')
		return(num)
	elif tp==';':
		return(max(factors(number(array[0]))))
	elif tp=='ᴙ':
		num=str(number(array[0]))[::-1]
		while num[0]=='0':num=num[1:]
		return(number(num))
	elif tp=='ﬁ':
		num=str(number(array[0]))
		return(number('(φ^'+num+'-Φ^'+num+')/(5^.5)'))


def drange(start, stop, step):
	r = start
	while r < stop:
		yield r
		r += step

def choose(n,k):return(factorial(n)/(factorial(n-k)*factorial(k)))

def product(lst):
	product=1
	for i in lst:product*=i
	return(product)

def factors(num):
	factors=[1]
	for i in range(2,num):
		if float(num)/i == int(num/i):factors+=[i]
	return(factors)

def derivative(formula,num,degree=1):
	#print('derivative debug start:',formula,num)
	h=epsilon
	c=0
	d=0
	cf=fdc[degree-1]
	for i in range(-4,5):
		if i < 0:c=(-1)**(degree%2)*cf[-i]
		else: c=cf[i]
		values[-1]=num+i*h
		d+=c*number(formula)
	#print('derivative debug end:',d/(h**degree))
	return(d/(h**degree))

def rationalize(num,mx=100):
	rnum=num
	foundnum=False
	#constants
	for c in values:
		for a in range(-mx,mx):
			if abs(num-c*a)<=epsilon:
				rnum=c*a
				foundnum=True
	#fractions
	if not foundnum:
		for a in range(-mx,mx):
			for b in range(1,mx):
				if abs(rnum.real-float(a)/b)<=epsilon:
					rnum+=float(a)/b-rnum.real
					foundnum=True
				if abs(rnum.imag-1j*float(a)/b)<=epsilon:
					rnum+=1j*float(a)/b-1j*rnum.imag
					foundnum=True
	#Complex
	if abs(rnum.imag)<=epsilon:rnum=rnum.real
	elif abs(rnum.real)<=epsilon:rnum=1j*rnum.imag
	#integers
	if rnum.imag!=0:
		if (abs(rnum.real-round(rnum.real))<=epsilon):rnum+=round(rnum.real)-rnum.real
		if (abs(rnum.imag-round(rnum.imag))<=epsilon):rnum+=1j*round(rnum.imag)-1j*rnum.imag
	elif abs(rnum-round(rnum))<=epsilon:rnum=round(rnum)
	return(rnum)

def number(st):
	global constants
	global values
	#print('number debug start:',st)
	if 'L' in st:values[constants.index('L')]=len(stack)
	built=""
	nindex=0
	parsing=True
	endbracket=False
	for i in range(len(st)):
		#print(built)
		char=st[i]
		if parsing:
			if char == 's':
				if len(built)>0 and built[-1] in "0123456789j.)" :built+='*'
				tst=st[i+1:]
				tbuilt=''
				for tchar in tst:
					if tchar == '-' and len(tbuilt)==0 or tchar in '0123456789':tbuilt+=tchar
					else:break
				if len(tbuilt)!=0:
					built+='('+str(stack[number(tbuilt)%len(stack)])+')'
					parsing=False
					nindex=i+len(tbuilt)
					tbuilt=''
				else:built+='('+str(stack[0])+')'
			if char == 'ƨ':
				if len(built)>0 and built[-1] in "0123456789j.)" :built+='*'
				tst=st[i+1:]
				tbuilt=''
				for tchar in tst:
					if tchar == '-' and len(tbuilt)==0 or tchar in '0123456789':tbuilt+=tchar
					else:break
				if len(tbuilt)!=0:
					built+='('+str(stack[(-number(tbuilt))%len(stack)])+')'
					parsing=False
					nindex=i+len(tbuilt)
					tbuilt=''
				else:built+='('+str(stack[-1])+')'
			elif char == 'ᵢ':
				if len(built)>0 and built[-1] in "0123456789j.)" :built+='*'
				built+='('+str(input('input: '))+')'
			elif char in constants:
				if len(built)>0 and built[-1] in "0123456789j.)" :built+='*'
				built+='('+str(values[constants.index(char)])+')'
			elif char in functions:
				#print(char)
				if len(built)>0 and built[-1] in "0123456789j.)" :built+='*'
				built+='('+str(rationalize(function(char,arrayAfter(st,i,' ','}',smode=True,fbrackets=True))))+')'
				parsing=False
				nindex=nextindex(st,'}',i,fbrackets=True)
			elif char in eops:
				built+=eopequals[eops.index(char)]
			elif char == '_':concatinate=True
			elif char in operations:
				built+=char
			elif char == "(":
				if len(built)>0 and built[-1] in "0123456789j.)" :built+='*'
				built+=char
			elif char in "0123456789j.)":
				if len(built)>0 and built[-1] == ')' and char!=')':built+='*'
				built+=char
			elif char == '÷':
				built='('+built+')/('
				endbracket=True
		elif i == nindex:parsing=True
	#print('number debug end:',st,built,eval(built))
	if endbracket:built+=')'

	num=eval(built)
	built=""
	return(num)


def before(st,s1,s2):
	s1in=s1 in st
	s2in=s2 in st
	#print('before debug:',s1in,s2in)
	if not s2in:return(True)
	elif not s1in:return(False)
	elif nextindex(st,s1)<nextindex(st,s2):return(True)

def whitespace(st,index=0):
	for i in range(index,len(st)):
		if st[i] in whitespaces:return(i)
	return(len(st))

def arrayAfter(code,parser,seperator=',',end=']',start='[',smode=False,fbrackets=False,level=0):
	array=[]
	tcode=code[parser+1:nextindex(code,end,parser,fbrackets=fbrackets)]
	#print('arrayafter debug:',code[parser],tcode,smode,fbrackets)
	tnum=''
	for i in range(len(tcode)):
		if tcode[i]==seperator:
			if smode:array+=[tnum]
			else:array+=[number(tnum)]
			tnum=''
		elif tcode[i] in end and not fbrackets:
			if smode:array+=[tnum]
			else:array+=[number(tnum)]
			tnum=''
			break
		elif tcode[i] in end and fbrackets and level==0:
			if smode:array+=[tnum]
			else:array+=[number(tnum)]
			tnum=''
			break
		elif tcode[i]=='}' and fbrackets and level!=0:
			level-=1
			tnum+=tcode[i]
		elif fbrackets and tcode[i] in functions:
			level+=1
			tnum+=tcode[i]
		else:tnum+=tcode[i]
		#print(tcode[i],level,tnum,tcode[i] in end)
	
	if len(tnum)!=0:
		if smode:array+=[tnum]
		else:array+=[number(tnum)]
		tnum=''
	#print('arrayafter debug:',array)
	return(array)


def arrayLen(array):
	#print('ARRAYLEN:',array)	
	l=0
	for i in array:
		l+=len(str(i))+1
	return(l)

def isPrime(num):
	if num < 2 or (num>2 and num%2==0): return(False)
	for i in range(2,int(num//2)+1):
		if num%i==0:return(False)
	return(True)

def element(tp,num):
	if tp=='P':return(isPrime(num))
	elif tp=='N':return(num>0 and num//1==num)
	elif tp=='Ṇ':return(num>=0 and num//1==num)
	elif tp=='Z':return(num//1==num)
	elif tp=='R':return(num.real==num)
	elif tp=='S':return(num in stack)
	elif tp=='□':return(num**.5==num**.5//1)

def boolean(tp,formula,num,comp):
	global constants
	global values

	bl=False

	values[-1]=num
	num=number(formula)
	if comp not in sets:comp=number(comp)

	if tp=='>':bl=(num>comp)
	elif tp=='≥':bl=(num>=comp)
	elif tp=='<':bl=(num<comp)
	elif tp=='≤':bl=(num<=comp)
	elif tp=='=':bl=(num==comp)
	elif tp=='≠':bl=(num!=comp)
	elif tp==':':bl=((comp/num)%1==0)
	elif tp=='≈':bl=(num//1==comp//1)
	elif tp==',':bl=(str(num) in str(comp))
	elif tp=='\'':bl=(str(comp) in str(num))
	elif tp=='ϵ':bl=element(comp,num)
	return(bl)

def nextindex(st,search,index=0,fbrackets=False,level=0):
	for i in range(index+1,len(st)):
		if st[i] in functions and fbrackets:level+=1
		elif st[i] in search:
			if level==0 or not fbrackets:return(i)
			else:level-=1
	return(len(st))
	

def runcmd(cmd,code,parser):
	global stack
	global constants
	global values
	global cfunct
	global Pstack

	if cmd=='[':#Insert
		stack+=arrayAfter(code,parser)
		return(nextindex(code,']',parser)-parser)
	elif cmd=='ᵢ':
		inp=input('input: ')
		if ',' in inp:
			inp='['+inp+']'
			inp=arrayAfter(inp,0)
			stack+=inp
		else:stack+=[number(inp)]
	elif cmd == 'O':#Operation on whole stack
		formula=code[parser+1:whitespace(code,parser)]
		for i in range(len(stack)):
			constants=['n']+['ṅ']+constants
			values=[i]+[i+1]+values
			values[-1]=stack[i]
			stack[i]=number(formula)
		constants=constants[2:]
		values=values[2:]
		return(whitespace(code,parser)-parser)
	elif cmd in operations or cmd in eops or cmd in constants or cmd in "0123456789j.(":#Operation on whole stack
		if len(stack)!=0:
			formula='x'+'*'*(cmd in "0123456789j.(")+code[parser:whitespace(code,parser)]
			for i in range(len(stack)):
				constants=['n']+['ṅ']+constants
				values=[i]+[i+1]+values
				values[-1]=stack[i]
				stack[i]=number(formula)
			constants=constants[2:]
			values=values[2:]
		else:
			formula=code[parser:whitespace(code,parser)]
			stack=[number(formula)]
		return(whitespace(code,parser)-parser)
	elif cmd=='Σ':#Sum stack
		stack=[sum(stack)]
	elif cmd=='∏':#Product stack
		stack=[product(stack)]
	elif cmd in functions:#Function on whole stack
		formula=code[parser:whitespace(code,parser)]
		try:
			for i in range(len(stack)):
				values[-1]=stack[i]
				stack[i]=number(formula)
			return(len(formula))
		except:
			for i in range(len(stack)):
				values[-1]=stack[i]
				stack[i]=number(formula[0]+'x}')
	elif cmd == 'B':#Boolean operatrion on whole stack
		formula=code[parser+1:nextindex(code,booleans,parser+1)]
		tp=code[nextindex(code,booleans,parser+1)]
		comp=code[nextindex(code,booleans,parser+1)+1:whitespace(code,parser)]
		tstack=[]
		for i in stack:
			constants=['n']+['ṅ']+constants
			values=[i]+[i+1]+values
			tstack+=boolean(tp,formula,i,comp)
		constants=constants[2:]
		values=values[2:]
		stack=[i for i in tstack]
		return(whitespace(code,parser)-parser)
	elif cmd == 'Ƀ':#Manipulate stack according to boolean Ƀx⁻ϵ□
		formula=code[parser+1:nextindex(code,booleans,parser+1)]
		tp=code[nextindex(code,booleans,parser+1)]
		comp=code[nextindex(code,booleans,parser+1)+1:whitespace(code,parser)]
		if 'ᵢ' in formula:formula=str(number(formula))
		if 'ᵢ' in comp:comp=str(number(comp))
		tstack=[]
		for i in stack:
			constants=['n']+['ṅ']+constants
			values=[i]+[i+1]+values
			if boolean(tp,formula,i,comp): tstack+=[i]
		constants=constants[2:]
		values=values[2:]
		stack=[i for i in tstack]
		return(whitespace(code,parser)-parser)
	elif cmd in booleans:#Boolean operatrion on whole stack
		formula='x'
		tp=cmd
		comp=code[parser+1:whitespace(code,parser)]
		stack=[i for i in stack if boolean(tp,formula,i,comp)]
		#print(formula,tp,comp)
		return(whitespace(code,parser)-parser)
	elif cmd=='N':#List of natural numbers without zero
		num=code[parser+1:whitespace(code,parser)]
		if num[0]!='ᵢ':
			stack+=list(range(1,1+int(number(num))))
			return(whitespace(code,parser)-parser)
		else:
			stack+=list(range(1,1+int(number(num[0]))))
			return(1)
	elif cmd=='Ṇ':#List of natural numbers with zero
		stack+=list(range(0,1+int(number(code[parser+1:whitespace(code,parser)]))))
		return(whitespace(code,parser)-parser)
	elif cmd=='Z':#List of integers
		array=arrayAfter(code,parser,',',whitespaces)
		#print("ARRAY:",array)
		stack+=list(range(array[0],array[1]))
		return(whitespace(code,parser)-parser)
	elif cmd=='Ż':#List of integers
		array=arrayAfter(code,parser,',',whitespaces)
		#print("ARRAY:",array)
		stack+=list(range(array[0],array[1]+1))
		return(whitespace(code,parser)-parser)
	elif cmd=='R':#Range of numbers
		array=arrayAfter(code,parser,',',whitespaces)
		stack+=list(drange(array[0],array[1],array[2]))
		return(whitespace(code,parser)-parser)
	elif cmd=='Ṛ':#Inclusive range of numbers
		array=arrayAfter(code,parser,',',whitespaces)
		stack+=list(drange(array[0],array[1]+array[2],array[2]))
		return(whitespace(code,parser)-parser)	
	elif cmd=='r':#List of random numbers
		array=arrayAfter(code,parser,',',whitespaces)
		#print(array)
		if len(array)==1:stack+=[randint(0,array[0])]
		elif len(array)==2:stack+=[randint(0,array[1]) for _ in range(array[0])]
		else:stack+=[randint(array[1],array[2]) for _ in range(array[0])]
		return(whitespace(code,parser)-parser)
	elif cmd=='ᵴ':#Swap elements
		array=arrayAfter(code,parser,',',whitespaces)

		tstack=[j for j in stack]
		for i in range(len(array)):
			stack[array[i]]=tstack[array[(i+1)%len(array)]]

		return(whitespace(code,parser)-parser)
	elif cmd=='$':#Sort stack
		stack=sorted(stack)
	elif cmd=='ᴙ':#Reverse stack
		stack=stack[::-1]
	elif cmd=='F':#For loop
		itr=number(code[parser+1:nextindex(code,'{',parser)])
		#print(itr)
		tcode=code[nextindex(code,'{',parser)+1:nextindex(code,'Ⅎ',parser)]
		#print(tcode)
		for _ in range(itr):runCode(tcode,False)
		return(nextindex(code,'Ⅎ',parser)-parser)
	elif cmd=='Đ':#Clear stack
		array=arrayAfter(code,parser,',',whitespaces,smode=True)
		if len(array)==0:
			stack=[]
		else:
			for i in array:
				del stack[number(i)]
		return(arrayLen(array))
	elif cmd=='ḟ':#Define function
		cfunct=code[parser+1:whitespace(code,parser)]
		if cfunct[0] != 'ᵢ':
			if 'x' not in cfunct:
				constants=['f']+constants
				values=[number(cfunct)]+values
			return(whitespace(code,parser)-parser)
		else:
			cfunct=str(input('input: '))
			if 'x' not in cfunct:
				constants=['f']+constants
				values=[number(cfunct)]+values
			return(1)
	elif cmd=='Ī':#If statement
		formula=code[parser+1:nextindex(code,booleans,parser+1)]
		tp=code[nextindex(code,booleans,parser+1)]
		comp=code[nextindex(code,booleans,parser+1)+1:nextindex(code,'{',parser)]
		els=False
		if 'Ǝ' in code[parser:]:els=before(code[parser+1:],'Ǝ','Ī')

		if boolean(tp,formula,0,comp):runCode(code[nextindex(code,'{',parser)+1:nextindex(code,'Ɨ',parser)],False)
		elif els:runCode(code[nextindex(code,'Ɨ',parser)+1:nextindex(code,'Ǝ',parser)],False)
		if not els:return(nextindex(code,'Ɨ',parser)-parser)
		else:return(nextindex(code,'Ǝ',parser)-parser)
	elif cmd=='W':#While loop
		formula=code[parser+1:nextindex(code,booleans,parser+1)]
		tp=code[nextindex(code,booleans,parser+1)]
		comp=code[nextindex(code,booleans,parser+1)+1:nextindex(code,'{',parser)]
		if 'ᵢ' in comp: comp=str(number(comp))
		while boolean(tp,formula,0,comp):runCode(code[nextindex(code,'{',parser)+1:nextindex(code,'M',parser)],False)
		return(nextindex(code,'M',parser)-parser)
	elif cmd=='Ẇ':#While loop
		formula=code[parser+1:nextindex(code,booleans,parser+1)]
		tp=code[nextindex(code,booleans,parser+1)]
		comp=code[nextindex(code,booleans,parser+1)+1:nextindex(code,'{',parser)]
		while True:
			runCode(code[nextindex(code,'{',parser)+1:nextindex(code,'Ṁ',parser)],False)
			if not boolean(tp,formula,0,comp):break
		return(nextindex(code,'Ṁ',parser)-parser)
	elif cmd=='E':
		array=arrayAfter(code,parser,',',whitespaces,smode=True,fbrackets=True)
		if len(array)==1:
			values[-1]=stack[0]
			stack[0]=number(array[0])
		elif len(array)==2:
			values[-1]=stack[int(number(array[0]))]
			stack[int(number(array[0]))]=number(array[1])
		else:
			formula=code[parser+1:nextindex(code,booleans,parser+1)]
			tp=code[nextindex(code,booleans,parser+1)]
			comp=code[nextindex(code,booleans,parser+1)+1:nextindex(code,',',parser)]
			if boolean(tp,formula,stack[number(array[1])],comp):
				stack[int(number(array[1]))]=number(array[2])
		return(arrayLen(array))
	elif cmd=='←':
		itr=code[parser+1:whitespace(code,parser)]
		if len(itr)==0:itr=1
		else:itr==number(itr)
		stack=[stack[(i+itr)%len(stack)] for i in range(len(stack))]
		return(whitespace(code,parser)-parser)
	elif cmd=='→':
		itr=code[parser+1:whitespace(code,parser)]
		if len(itr)==0:itr=1
		else:itr==number(itr)
		stack=[stack[(i-itr)%len(stack)] for i in range(len(stack))]
		return(whitespace(code,parser)-parser)
	elif cmd=='p':
		try:itr=str(number(code[parser+1:whitespace(code,parser)]))
		except:itr=str(number(code[parser+1]))
		try:
			stack=stack[:len(stack)-int(number(itr))]
			return(len(itr))
		except:
			stack=stack[:len(stack)-1]
	elif cmd=='q':
		try:itr=str(number(code[parser+1:whitespace(code,parser)]))
		except:itr=str(number(code[parser+1]))
		try:
			stack=stack[int(number(itr)):]
			return(len(itr))
		except:
			stack=stack[1:]
	elif cmd=='¹':
		stack+=[1]
	elif cmd=='⁰':
		stack+=[0]
	elif cmd=='Ṗ':
		array=arrayAfter(code,parser,',',whitespaces,smode=False,fbrackets=True)
		if len(array)==0:print(stack[0])
		else:print(stack[int(array[0])])
		Pstack=False
		return(arrayLen(array))
	elif cmd=='Ṕ':
		array=arrayAfter(code,parser,',',whitespaces,smode=False,fbrackets=True)
		if len(array)==0:print(stack[-1])
		else:print(stack[int(array[0])])
		Pstack=False
		return(arrayLen(array))
	elif cmd=='Ƥ':
		print(number(code[parser+1:nextindex(code,'Ƥ',parser)]))
		Pstack=False
		return(nextindex(code,'Ƥ',parser)-parser)
	elif cmd=='©':
		stack=stack+stack
	elif cmd==':':
		tstack=[]
		for i in stack:
			tstack+=factors(i)
		stack=[i for i in tstack]
	elif cmd=='ṁ':
		stack=max(stack)
	elif cmd=='m':
		stack=min(stack)
	elif cmd=='Y':
		tstack=[]
		for i in stack:
			tstack+=[number(j) for j in str(i)]
		stack=[i for i in tstack]
	return(0)

def runCode(code,pstack=True):
	parser=0
	parsing=True
	while parsing:
		cmd=code[parser]
		#print('COMMAND:',cmd)
		parser+=runcmd(cmd,code,parser)
		parser+=1
		if parser>=len(code):parsing=False
	#
	if pstack and Pstack:print(stack)

try:runCode(rcode)
except:print('ParseError: There was an error parsing the program')
	
