(********************************************************************************)
(*                                                                              *)
(*                                  P A P P E                                   *)
(*                        PAscal Powerful Physics Engine                        *)
(*                                                                              *)
(********************************************************************************)
{

BeRo's Open Source License (BOSL) - Version 1.0
 
Copyright (c) 2008, 2009 Benjamin Rosseaux. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in
   the documentation and/or other materials provided with the
   distribution.

3. Redistributions of any form whatsoever must retain the following
   acknowledgment:
       "This product includes software developed by
        Benjamin Rosseaux (http://www.rosseaux.com/)."

4. You may embed this software within an executable of yours (by linking),
   provided that in your software will appear the following acknowledgment:
       "This product includes software developed by
        Benjamin Rosseaux (http://www.rosseaux.com/)."

5. It is not allowed to remove or change this license of this software, 
   parts of it or from a modified version. You may use this license for 
   previous releases of this software instead of the license that they 
   came with, at your option.

6. The original author Benjamin Rosseaux has the right to change the 
   license all the time for new versions and new releases of this software.

THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

}
(********************************************************************************)
(*                                                                              *)
(* The Initial Developer of the original code is Benjamin Rosseaux              *)
(* Portions created by the initial developer are copyright (C) 2006-2009        *)
(* the Initial Developer. All Rights Reserved.                                  *)
(*                                                                              *)
(* Contributor(s):                                                              *)
(*  Benjamin Rosseaux <benjamin@rosseaux.com>                                   *)
(*                                                                              *)
(********************************************************************************)
unit PAPPE;
{$ifdef FPC}
 {$mode delphi}
 {$warnings off}
 {$hints off}
 {$ifdef CPUI386}
  {$define CPU386}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
{$else}
 {$define LITTLE_ENDIAN}
 {$ifndef CPU64}
  {$define CPU32}
 {$endif}
 {$optimization on}
{$endif}
{$undef caninline}
{$undef canx86simd}
{$ifdef VER180}
 {$define caninline}
 {$ifdef CPU386}
  {$define canx86simd}
 {$endif}
 {$finitefloat off}
{$endif}
{$ifdef FPC}
 {$define caninline}
 {$ifdef CPU386}
  {$define canx86simd}
 {$endif}
{$else}
 {$safedivide off}
{$endif}
{$extendedsyntax on}
{$writeableconst on}
{$varstringchecks on}
{$typedaddress off}
{$overflowchecks off}
{$rangechecks off}
{$realcompatibility off}
{$openstrings on}
{$longstrings on}
{$booleval off}

interface

{$ifdef CPUX86_64}
{-$fpumode sse64}
{$endif}

{$ifdef physicsshowcontacts}
uses {$ifdef CPU64}Math,{$endif}OpenGL;
{$else}
{$ifdef CPU64}
 uses Math;
{$endif}
{$endif}

type PPhysicsFloat=^TPhysicsFloat;
     TPhysicsFloat={$ifdef physicsdouble}double{$else}single{$endif};

const BodyMesh=0;
      BodyBox=1;
      BodySphere=2;
      BodyCylinder=3;
      BodyCapsule=4;
      BodyPlane=5;
      BodyHeightmap=6;
      BodyConvexHull=7;

      BodyUserBase=1000;

      BodyStart=BodyMesh;
      BodyEnd=BodyConvexHull;

      JointNone=-1;
      JointBase=0;
      JointBall=1;
      JointHinge=2;
      JointUniversal=3;
      JointUser=4;

      JointStart=JointBase;
      JointEnd=JointUser;

      JOINT_DIST=10.0;

      ConstraintNone=-1;
      ConstraintMaxDistance=0;
      ConstraintPoint=1;
      ConstraintWorldPoint=2;
      ConstraintVelocity=3;
      ConstraintUser=4;

      ConstraintStart=ConstraintMaxDistance;
      ConstraintEnd=ConstraintUser;

      CollideMaxContacts=4096;
      CollideMaxObjects=32;

      hwmBRUTEFORCE=0;
      hwmAABB=1;
      hwmSPHERE=2;

      sapwmAXISALL=-1;
      sapwmAXISX=0;
      sapwmAXISY=1;
      sapwmAXISZ=2;
      sapwmAXISAUTO=3;
      sapwmOFF=4;

      chgwmINCREMENTAL=0;
      chgwmRANDOMIZEDINCREMENTAL=1;
      chgwmSIMPLEBEROHULL=2;
      chgwmBEROHULL=3;

      fCI3=1/3;
      fCI6=1/6;

      EPSILON:TPhysicsFloat=1e-12;
{$ifdef CPU64}
      INFINITY:TPhysicsFloat=1e+18;
{$else}
      INFINITY:TPhysicsFloat=1e+30;
{$endif}

      AABBEPSILON:TPhysicsFloat=0.1;

      DEG2RAD:TPhysicsFloat=pi/180;
      RAD2DEG:TPhysicsFloat=180/pi;

      DISTANCE_EPSILON:TPhysicsFloat=1e-12;

      MemoryIncBits=8;
      MemoryInc=1 shl MemoryIncBits;
      MemoryIncMask=MemoryInc-1;

type PPhysicsFloatArray=^TPhysicsFloatArray;
     TPhysicsFloatArray=array[0..65535] of TPhysicsFloat;
     
     PPhysicsVector2=^TPhysicsVector2;
     TPhysicsVector2=record
      case boolean of
       true:(x,y:TPhysicsFloat);
       false:(xy:array[0..1] of TPhysicsFloat);
     end;

     PPhysicsVector3=^TPhysicsVector3;
     TPhysicsVector3=packed record
      case boolean of
       true:(x,y,z:TPhysicsFloat);
       false:(xyz:array[0..2] of TPhysicsFloat);
     end;

     PPhysicsVector4=^TPhysicsVector4;
     TPhysicsVector4=packed record
      case boolean of
       true:(x,y,z,w:TPhysicsFloat);
       false:(xyzw:array[0..3] of TPhysicsFloat);
     end;

     PPhysicsPlane=^TPhysicsPlane;
     TPhysicsPlane=packed record
      case integer of
       0:(a,b,c,d:TPhysicsFloat);
       1:(Normal:TPhysicsVector3;
          Distance:TPhysicsFloat);
       2:(xyzw:TPhysicsVector4);
     end;

     PPhysicsQuaternion=^TPhysicsQuaternion;
     TPhysicsQuaternion=record
      case boolean of
       true:(w,x,y,z:TPhysicsFloat);
       false:(wxyz:array[0..3] of TPhysicsFloat);
     end;

     PPhysicsSphereCoords=^TPhysicsSphereCoords;

     TPhysicsSphereCoords=record
      Radius,Theta,Phi:TPhysicsFloat;
     end;

     PPhysicsEuler=^TPhysicsEuler;

     TPhysicsEuler=record
      Pitch,Yaw,Roll:TPhysicsFloat;
     end;

     PPhysicsMatrix2x2=^TPhysicsMatrix2x2;
     TPhysicsMatrix2x2=array[0..1,0..1] of TPhysicsFloat;

     PPhysicsMatrix2x2ex=^TPhysicsMatrix2x2ex;
     TPhysicsMatrix2x2ex=array[0..3] of TPhysicsFloat;

     PPhysicsMatrix3x3=^TPhysicsMatrix3x3;
     TPhysicsMatrix3x3=array[0..2,0..2] of TPhysicsFloat;

     PPhysicsMatrix3x3ex=^TPhysicsMatrix3x3ex;
     TPhysicsMatrix3x3ex=array[0..8] of TPhysicsFloat;

     PPhysicsMatrix4x4=^TPhysicsMatrix4x4;
     TPhysicsMatrix4x4=array[0..3,0..3] of TPhysicsFloat;

     PPhysicsMatrix4x4ex=^TPhysicsMatrix4x4ex;
     TPhysicsMatrix4x4ex=array[0..15] of TPhysicsFloat;

     PPhysicsAABB=^TPhysicsAABB;

     TPhysicsAABB=record
      Min,Max:TPhysicsVector3;
     end;

     PPhysicsSphere=^TPhysicsSphere;

     TPhysicsSphere=record
      Center:TPhysicsVector3;
      Radius:TPhysicsFloat;
     end;

     TPhysicsSortCompareFunction=function(const a,b:pointer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     TPhysicsBoxCorners=array[0..7] of TPhysicsVector3;

     TPhysicsSegment=record
      Origin,Delta:TPhysicsVector3;
     end;

     TPhysicsSegmentTriangle=record
      Origin,Edge0,Edge1,Edge2:TPhysicsVector3;
     end;

     PPhysicsLineContactSet=^TPhysicsLineContactSet;

     TPhysicsLineContactSet=record
      Points:array[0..7] of TPhysicsVector3;
      Count:integer;
     end;

     PPPhysicsRigidBody=^PPhysicsRigidBody;
     PPhysicsRigidBody=^TPhysicsRigidBody;
     PPhysicsRagDoll=^TPhysicsRagDoll;

     PPhysicsQuadVertices=^TPhysicsQuadVertices;
     TPhysicsQuadVertices=array[0..3] of TPhysicsVector3;

     PPhysicsTriangleVertices=^TPhysicsTriangleVertices;
     TPhysicsTriangleVertices=array[0..2] of TPhysicsVector3;

     PPhysicsPlanes=^TPhysicsPlanes;
     TPhysicsPlanes=array[0..5] of TPhysicsPlane;

     PPhysicsFastCheckPlanes=^TPhysicsFastCheckPlanes;
     TPhysicsFastCheckPlanes=array[0..2] of TPhysicsPlane;

     PPhysicsTriangle=^TPhysicsTriangle;

     TPhysicsTriangle=record
      Vertices:TPhysicsTriangleVertices;
      FastTriangleCheckPlanes:TPhysicsFastCheckPlanes;
      Plane:TPhysicsPlane;
      TransformedVertices:TPhysicsTriangleVertices;
      TransformedFastTriangleCheckPlanes:TPhysicsFastCheckPlanes;
      TransformedPlane:TPhysicsPlane;
     end;

     PPhysicsTriangles=^TPhysicsTriangles;
     TPhysicsTriangles=array[0..0] of TPhysicsTriangle;

     PPPhysicsTriangles=^TPPhysicsTriangles;
     TPPhysicsTriangles=array[0..0] of PPhysicsTriangle;

     PPhysicsObject=^TPhysicsObject;

     PPhysicsContact=^TPhysicsContact;

     TPhysicsContact=record
      Point:TPhysicsVector3;
      Normal:TPhysicsVector3;
      Depth:TPhysicsFloat;
      case boolean of
       false:(ContactObject:PPhysicsObject;);
       true:(SenderObject:PPhysicsObject;);
     end;

     PPPhysicsContact=^PPhysicsContact;

     PPhysicsContacts=^TPhysicsContacts;
     TPhysicsContacts=array[0..0] of TPhysicsContact;

     PPPhysicsContacts=^TPPhysicsContacts;
     TPPhysicsContacts=array[0..0] of PPhysicsContact;

     PPhysicsObjectSweepAndPruneAxis=^TPhysicsObjectSweepAndPruneAxis;
     TPhysicsObjectSweepAndPruneAxis=record
      Previous,Next:PPhysicsObject;
     end;

     PPhysicsObjectSweepAndPruneAllAxis=^TPhysicsObjectSweepAndPruneAllAxis;
     TPhysicsObjectSweepAndPruneAllAxis=array[0..2] of TPhysicsObjectSweepAndPruneAxis;

     PPhysicsObjectSweepAndPrunePair=^TPhysicsObjectSweepAndPrunePair;
     TPhysicsObjectSweepAndPrunePair=record
      WithObject:PPhysicsObject;
      Flags:integer;
     end;

     PPhysicsObjectSweepAndPrunePairs=^TPhysicsObjectSweepAndPrunePairs;
     TPhysicsObjectSweepAndPrunePairs=array[0..0] of TPhysicsObjectSweepAndPrunePair;

     PPhysicsObjectSweepAndPruneCacheItem=^TPhysicsObjectSweepAndPruneCacheItem;
     TPhysicsObjectSweepAndPruneCacheItem=integer;

     PPhysicsObjectSweepAndPruneCache=^TPhysicsObjectSweepAndPruneCache;
     TPhysicsObjectSweepAndPruneCache=array[0..0] of TPhysicsObjectSweepAndPruneCacheItem;

     PPhysicsObjectMesh=^TPhysicsObjectMesh;
     PPhysicsObjectMeshs=^TPhysicsObjectMeshs;

     TPhysicsObjectMesh=record
      NumMeshs:integer;
      Meshs:PPhysicsObjectMeshs;

      NumTriangles:integer;
      AllTriangles:integer;
      Triangles:PPhysicsTriangles;

      AABB:TPhysicsAABB;
      Sphere:TPhysicsSphere;
      MeshPlane:TPhysicsPlane;
     end;

     TPhysicsObjectMeshs=array[0..65535] of PPhysicsObjectMesh;

     TPhysicsObjectCollisionProc=function(ReceiverObject:PPhysicsObject;var Contect:TPhysicsContact):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     PPhysicsObjectHeightMapData=^TPhysicsObjectHeightMapData;
     TPhysicsObjectHeightMapData=array[0..0] of TPhysicsFloat;

     TPhysicsObjectHeightMap=record
      Width,Height:integer;
      Data:PPhysicsObjectHeightMapData;
      Size:TPhysicsVector3;
      CellSize:TPhysicsVector3;
     end;

     TPhysicsObject=record
      Previous,Next:PPhysicsObject;

      ID:integer;

      BodyType:integer;
      CollisionBodyType:integer;

      HaveNewTransform:boolean;

      AABB:TPhysicsAABB;
      Sphere:TPhysicsSphere;
      ObjectPlane:TPhysicsPlane;

      TransformAABB:TPhysicsAABB;
      TransformSphere:TPhysicsSphere;

      Position:TPhysicsVector3;
      OldPosition:TPhysicsVector3;

      RigidBody:PPhysicsRigidBody;

      CollisionSender:boolean;
      CollisionReceiver:boolean;

      IsIdentity:boolean;
      Transform:TPhysicsMatrix4x4;
      InvTransform:TPhysicsMatrix4x4;

      OldModelView:TPhysicsMatrix4x4;
      OldInvModelView:TPhysicsMatrix4x4;
      OldTransform:TPhysicsMatrix4x4;
      OldInvTransform:TPhysicsMatrix4x4;

      Time:TPhysicsFloat;
      Frame:integer;

      RagDoll:PPhysicsRagDoll;

      NumMeshs:integer;
      Meshs:PPhysicsObjectMeshs;

      BoxMesh:TPhysicsObjectMesh;
      ConvexHullMesh:TPhysicsObjectMesh;

      SweepAndPruneAllAxis:TPhysicsObjectSweepAndPruneAllAxis;

      SweepAndPruneCache:PPhysicsObjectSweepAndPruneCache;
      SweepAndPruneCacheSize:integer;

      SweepAndPrunePairs:PPhysicsObjectSweepAndPrunePairs;
      SweepAndPrunePairsSize:integer;
      SweepAndPrunePairsCount:integer;

      OnCollision:TPhysicsObjectCollisionProc;

      HeightMap:TPhysicsObjectHeightMap;

      UserData:pointer;
     end;

     PPhysicsObjects=^TPhysicsObjects;
     TPhysicsObjects=array[0..0] of TPhysicsObject;

     PPPhysicsObjects=^TPPhysicsObjects;
     TPPhysicsObjects=array[0..0] of PPhysicsObject;

     PPhysicsObjectSweepAndPruneAxisList=^TPhysicsObjectSweepAndPruneAxisList;
     TPhysicsObjectSweepAndPruneAxisList=record
      First,Last:PPhysicsObject;
     end;

     PPhysicsObjectSweepAndPruneAxisLists=^TPhysicsObjectSweepAndPruneAxisLists;
     TPhysicsObjectSweepAndPruneAxisLists=array[0..2] of TPhysicsObjectSweepAndPruneAxisList;

     PPhysicsObjectSweepAndPrune=^TPhysicsObjectSweepAndPrune;
     TPhysicsObjectSweepAndPrune=record
      Lists:TPhysicsObjectSweepAndPruneAxisLists;
     end;

     PPhysicsCollide=^TPhysicsCollide;

     TPhysicsUserCollideProc=function(Instance:PPhysicsCollide;TheObject,WithObject:PPhysicsObject):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
     
     TPhysicsUserPointCollideProc=function(Instance:PPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     TPhysicsCollide=record
      AllContacts:integer;
      NumContacts:integer;
      Contacts:PPPhysicsContacts;

      AllObjects:integer;
      NumObjects:integer;
      Objects:PPPhysicsObjects;

      Counter:integer;

      Position:TPhysicsVector3;

      UserCollideProc:TPhysicsUserCollideProc;
      UserPointCollideProc:TPhysicsUserPointCollideProc;
                                   
      UserData:pointer;
     end;

     PPhysicsCollideProc=^TPhysicsCollideProc;

     TPhysicsCollideProc=record
      Proc:procedure(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
      SwapObjects:boolean;
     end;

     PPhysicsCollideProcs=^TPhysicsCollideProcs;
     TPhysicsCollideProcs=array[BodyStart..BodyEnd,BodyStart..BodyEnd] of TPhysicsCollideProc;

     PPhysicsCollidePointProc=^TPhysicsCollidePointProc;
     TPhysicsCollidePointProc=procedure(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     PPhysicsCollidePointProcs=^TPhysicsCollideProcs;
     TPhysicsCollidePointProcs=array[BodyStart..BodyEnd] of TPhysicsCollidePointProc;

     PPhysicsObjectRayIntersectionProc=^TPhysicsObjectRayIntersectionProc;
     TPhysicsObjectRayIntersectionProc=function(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
     
     TPhysicsObjectRayIntersectionUserProc=function(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3;UserData:pointer):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     PPhysicsObjectRayIntersectionProcs=^TPhysicsObjectRayIntersectionProcs;
     TPhysicsObjectRayIntersectionProcs=array[BodyStart..BodyEnd] of TPhysicsObjectRayIntersectionProc;

     PPhysicsJoint=^TPhysicsJoint;

     TPhysicsJointUserResponseProc=function(Instance:PPhysicsJoint;TimeToWork:TPhysicsFloat;Data:pointer):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     TPhysicsJoint=record
      Active:boolean;
      RigidBodies:array[0..1] of PPhysicsRigidBody;
      RestrictionPoints:array[0..1] of TPhysicsVector3;
      RestrictionMinDist:TPhysicsFloat;
      HaveDestroyMinVelocity:boolean;
      DestroyMinVelocity:TPhysicsFloat;
      case JointType:integer of
       JointBase:(
       );
       JointBall:(
        BallPoints:array[0..1] of TPhysicsVector3;
       );
       JointHinge:(
        HingePoints:array[0..1,0..1] of TPhysicsVector3;
        HingePoint:TPhysicsVector3;
        HingeAxis:array[0..1] of TPhysicsVector3;
        HingeiTransforms:array[0..1] of TPhysicsMatrix4x4;
       );
       JointUniversal:(
        UniversalPoints:array[0..1,0..1] of TPhysicsVector3;
        UniversalPoint:TPhysicsVector3;
        UniversalAxis:array[0..1] of TPhysicsVector3;
       );
       JointUser:(
        Data:pointer;
        UserResponseProc:TPhysicsJointUserResponseProc;
       );
     end;

     TPhysicsJointResponseProc=function(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     PPhysicsJoints=^TPhysicsJoints;
     TPhysicsJoints=array[0..0] of TPhysicsJoint;

     PPPhysicsJoints=^TPPhysicsJoints;
     TPPhysicsJoints=array[0..0] of PPhysicsJoint;

     PPhysicsJointResponseProcs=^TPhysicsJointResponseProcs;
     TPhysicsJointResponseProcs=array[JointStart..JointEnd] of TPhysicsJointResponseProc;

     PPhysicsConstraint=^TPhysicsConstraint;

     TPhysicsConstraintUserResponseProc=function(Instance:PPhysicsConstraint;TimeToWork:TPhysicsFloat;Data:pointer):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     TPhysicsConstraint=record
      Active:boolean;
      RigidBodies:array[0..1] of PPhysicsRigidBody;
      case ConstraintType:integer of
       ConstraintMaxDistance:(
        MaxDistance:TPhysicsFloat;
        MaxDistanceBodyPos0:TPhysicsVector3;
        MaxDistanceBodyPos1:TPhysicsVector3;
       );
       ConstraintPoint:(
        PointAllowedDistance:TPhysicsFloat;
        PointTimeScale:TPhysicsFloat;
        PointBodyPos0:TPhysicsVector3;
        PointBodyPos1:TPhysicsVector3;
       );
       ConstraintWorldPoint:(
        WorldPoint:TPhysicsVector3;
        WorldPointPointOnBody:TPhysicsVector3;
       );
       ConstraintVelocity:(
        Velocity:TPhysicsVector3;
        VelocityAngular:TPhysicsVector3;
        VelocityDo:boolean;
        VelocityDoAngular:boolean;
        VelocityCurrentRate:TPhysicsVector3;
        VelocityAngularCurrentRate:TPhysicsVector3;
       );
       ConstraintUser:(
        Data:pointer;
        UserResponseProc:TPhysicsConstraintUserResponseProc;
       );
     end;

     PPPhysicsConstraints=^TPPhysicsConstraints;
     TPPhysicsConstraints=array[0..0] of PPhysicsConstraint;

     TPhysicsConstraintResponseProc=function(var Instance:TPhysicsConstraint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

     PPhysicsConstraintResponseProcs=^TPhysicsConstraintResponseProcs;
     TPhysicsConstraintResponseProcs=array[ConstraintStart..ConstraintEnd] of TPhysicsConstraintResponseProc;

     PPPhysicsRigidBodies=^TPPhysicsRigidBodies;

     TPhysicsRigidBody=record
      ID:integer;

      OwnerObject:PPhysicsObject;

      Collide:PPhysicsCollide;

      Mass:TPhysicsFloat;
      InvMass:TPhysicsFloat;

      Restitution:TPhysicsFloat;
      Friction:TPhysicsFloat;
      Volume:TPhysicsFloat;

      CenterOfMass:TPhysicsVector3;

      Position:TPhysicsVector3;
      Orientation:TPhysicsMatrix3x3;

      Transform:TPhysicsMatrix4x4;
      InvTransform:TPhysicsMatrix4x4;
      OldTransform:TPhysicsMatrix4x4;
      OldInvTransform:TPhysicsMatrix4x4;

      Velocity:TPhysicsVector3;
      AngularVelocity:TPhysicsVector3;

      VelocityDamp:TPhysicsFloat;
      AngularVelocityDamp:TPhysicsFloat;
      AdditionalDamping:boolean;
      AdditionalDamp:TPhysicsFloat;
      VelocityAdditionalDamp:TPhysicsFloat;
      AngularVelocityAdditionalDamp:TPhysicsFloat;
      VelocityAdditionalDampThresholdSqr:TPhysicsFloat;
      AngularVelocityAdditionalDampThresholdSqr:TPhysicsFloat;

      Force:TPhysicsVector3;
      Torque:TPhysicsVector3;

      InertiaTensor:TPhysicsMatrix3x3;
      InvBodyInertiaTensor:TPhysicsMatrix3x3;
      InvWorldInertiaTensor:TPhysicsMatrix3x3;

      OldVelocity:TPhysicsVector3;
      OldAngularVelocity:TPhysicsVector3;
      OldPosition:TPhysicsVector3;
      OldOrientation:TPhysicsMatrix3x3;
      OldInvWorldInertiaTensor:TPhysicsMatrix3x3;

      DoGravitation:boolean;

      Frozen:boolean;
      FrozenTime:TPhysicsFloat;
      FrozenNumObjects:integer;

      AllJoints:integer;
      NumJoints:integer;
      Joints:PPPhysicsJoints;
      JointedRigidBodies:PPPhysicsRigidBodies;

      AllConstraints:integer;
      NumConstraints:integer;
      Constraints:PPPhysicsConstraints;

      Immovable:boolean;
      Static:boolean;

      UserData:pointer;
     end;

     PPhysicsRigidBodies=^TPhysicsRigidBodies;
     TPhysicsRigidBodies=array[0..0] of TPhysicsRigidBody;

     TPPhysicsRigidBodies=array[0..0] of PPhysicsRigidBody;

     TPhysicsRagDoll=record
      Dummy:byte;
     end;

     PPhysics=^TPhysics;

     TPhysics=record
      SweepAndPruneWorkMode:integer;
      SweepAndPruneAxis:integer;

      ConvexHullGenerationWorkMode:integer;
      ConvexHullGenerationLevelOfDetail:integer;
      ConvexHullGenerationPlaneSideCheckAtGeneration:boolean;
      ConvexHullGenerationPlaneSideCheckAtTest:boolean;

      Time:TPhysicsFloat;
      TimeStep:TPhysicsFloat;
      Gravitation:TPhysicsVector3;
      VelocityMax:TPhysicsFloat;
      VelocityThreshold:TPhysicsFloat;
      AngularVelocityMax:TPhysicsFloat;
      AngularVelocityThreshold:TPhysicsFloat;
      TimeToFrost:TPhysicsFloat;
      PenetrationSpeed:TPhysicsFloat;

      NumFirstIterations:integer;
      NumSecondIterations:integer;

      AllJoints:integer;
      NumJoints:integer;
      Joints:PPPhysicsJoints;

      AllRigidBodies:integer;
      NumRigidBodies:integer;
      RigidBodies:PPPhysicsRigidBodies;
      RigidBodyID:integer;

      AllConstraints:integer;
      NumConstraints:integer;
      Constraints:PPPhysicsConstraints;

      ObjectFirst,ObjectLast:PPhysicsObject;
      ObjectID:integer;

      SweepAndPrune:TPhysicsObjectSweepAndPrune;
     end;

const Vector2Origin:TPhysicsVector2=(x:0;y:0);
      Vector2XAxis:TPhysicsVector2=(x:1;y:0);
      Vector2YAxis:TPhysicsVector2=(x:0;y:1);
      Vector2ZAxis:TPhysicsVector2=(x:0;y:0);

      Vector3Origin:TPhysicsVector3=(x:0;y:0;z:0);
      Vector3XAxis:TPhysicsVector3=(x:1;y:0;z:0);
      Vector3YAxis:TPhysicsVector3=(x:0;y:1;z:0);
      Vector3ZAxis:TPhysicsVector3=(x:0;y:0;z:1);

      Vector4Origin:TPhysicsVector4=(x:0;y:0;z:0;w:1);
      Vector4XAxis:TPhysicsVector4=(x:1;y:0;z:0;w:1);
      Vector4YAxis:TPhysicsVector4=(x:0;y:1;z:0;w:1);
      Vector4ZAxis:TPhysicsVector4=(x:0;y:0;z:1;w:1);

      Matrix2x2Identity:TPhysicsMatrix2x2=((1,0),(0,1));
      Matrix2x2Null:TPhysicsMatrix2x2=((0,0),(0,0));

      Matrix3x3Identity:TPhysicsMatrix3x3=((1,0,0),(0,1,0),(0,0,1));
      Matrix3x3Null:TPhysicsMatrix3x3=((0,0,0),(0,0,0),(0,0,0));

      Matrix4x4Identity:TPhysicsMatrix4x4=((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1));
      Matrix4x4Null:TPhysicsMatrix4x4=((0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0));

      QuaternionIdentity:TPhysicsQuaternion=(w:1;x:0;y:0;z:0);

var PhysicsInstance:PPhysics;

function GetSign(x:TPhysicsFloat):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Min(a,b:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Max(a,b:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function MinInt(a,b:integer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function MaxInt(a,b:integer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Power(number,exponent:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Tan(Angle:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function ArcTan2(y,x:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function ArcCos(x:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function ArcSin(x:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2(x,y:TPhysicsFloat):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3(x,y,z:TPhysicsFloat):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector3(v:TPhysicsVector4):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector4(x,y,z:TPhysicsFloat;w:TPhysicsFloat=1):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector4(v:TPhysicsVector3;w:TPhysicsFloat=1):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix3x3(const xx,xy,xz,yx,yy,yz,zx,zy,zz:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix3x3(const x,y,z:TPhysicsVector3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix3x3Raw(const xx,xy,xz,yx,yy,yz,zx,zy,zz:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4(const xx,xy,xz,xw,yx,yy,yz,yw,zx,zy,zz,zw,wx,wy,wz,ww:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix4x4(const x,y,z,w:TPhysicsVector4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix4x4Raw(const xx,xy,xz,xw,yx,yy,yz,yw,zx,zy,zz,zw,wx,wy,wz,ww:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Plane(a,b,c,d:TPhysicsFloat):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Plane(Normal:TPhysicsVector3;Distance:TPhysicsFloat):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Quaternion(w,x,y,z:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Euler(Pitch,Yaw,Roll:TPhysicsFloat):TPhysicsEuler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function Vector2Compare(const v1,v2:TPhysicsVector2):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Add(const v1,v2:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Sub(const v1,v2:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2ScalarMul(const v:TPhysicsVector2;s:TPhysicsFloat):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Dot(const v1,v2:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Neg(const v:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector2Scale(var v:TPhysicsVector2;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector2Scale(var v:TPhysicsVector2;sx,sy:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector2Mul(const v1,v2:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Length(const v:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Dist(const v1,v2:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2LengthSquared(const v:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Angle(const v1,v2,v3:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector2Normalize(var v:TPhysicsVector2); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Norm(const v:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector2Rotate(var v:TPhysicsVector2;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector2Rotate(var v:TPhysicsVector2;const Center:TPhysicsVector2;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector2MatrixMul(var v:TPhysicsVector2;const m:TPhysicsMatrix2x2); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector2Lerp(const v1,v2:TPhysicsVector2;w:TPhysicsFloat):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function Vector3Compare(const v1,v2:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Add(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Sub(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3ScalarMul(const v:TPhysicsVector3;s:TPhysicsFloat):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Dot(const v1,v2:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Cos(const v1,v2:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Cross(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Neg(const v:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector3Scale(var v:TPhysicsVector3;sx,sy,sz:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector3Scale(var v:TPhysicsVector3;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector3Mul(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Length(const v:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Dist(const v1,v2:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3LengthSquared(const v:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Angle(const v1,v2,v3:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector3Normalize(var v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Norm(const v:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector3RotateX(var v:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector3RotateY(var v:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector3RotateZ(var v:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector3MatrixMul(var v:TPhysicsVector3;const m:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector3MatrixMul(var v:TPhysicsVector3;const m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector3NormalMatrixMul(var v:TPhysicsVector3;const m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector3TermMatrixMul(const v:TPhysicsVector3;const m:TPhysicsMatrix3x3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector3TermMatrixMul(const v:TPhysicsVector3;const m:TPhysicsMatrix4x4):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector3Rotate(var v:TPhysicsVector3;const Axis:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector3Lerp(const v1,v2:TPhysicsVector3;w:TPhysicsFloat):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function Vector4Compare(const v1,v2:TPhysicsVector4):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Add(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Sub(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4ScalarMul(const v:TPhysicsVector4;s:TPhysicsFloat):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Dot(const v1,v2:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Cross(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Neg(const v:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector4Scale(var v:TPhysicsVector4;sx,sy,sz:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Vector4Scale(var v:TPhysicsVector4;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Vector4Mul(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Length(const v:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Dist(const v1,v2:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4LengthSquared(const v:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Angle(const v1,v2,v3:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector4Normalize(var v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Norm(const v:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector4RotateX(var v:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector4RotateY(var v:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector4RotateZ(var v:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector4MatrixMul(var v:TPhysicsVector4;const m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Vector4Rotate(var v:TPhysicsVector4;const Axis:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Vector4Lerp(const v1,v2:TPhysicsVector4;w:TPhysicsFloat):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function Matrix3x3RotateX(Angle:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3RotateY(Angle:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3RotateZ(Angle:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3Rotate(Euler:TPhysicsEuler):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix3x3Rotate(Angle:TPhysicsFloat;Axis:TPhysicsVector3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix3x3Scale(sx,sy,sz:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3Add(var m1:TPhysicsMatrix3x3;const m2:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3Sub(var m1:TPhysicsMatrix3x3;const m2:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3Mul(var m1:TPhysicsMatrix3x3;const m2:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3TermAdd(const m1,m2:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3TermSub(const m1,m2:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3TermMul(const m1,m2:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3ScalarMul(var m:TPhysicsMatrix3x3;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3TermScalarMul(const m:TPhysicsMatrix3x3;s:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3Transpose(var m:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3TermTranspose(const m:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3Determinant(const m:TPhysicsMatrix3x3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3Euler(const m:TPhysicsMatrix3x3):TPhysicsEuler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3SetColumn(var m:TPhysicsMatrix3x3;const c:integer;const v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3GetColumn(const m:TPhysicsMatrix3x3;const c:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3SetRow(var m:TPhysicsMatrix3x3;const r:integer;const v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3GetRow(const m:TPhysicsMatrix3x3;const r:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3Compare(const m1,m2:TPhysicsMatrix3x3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3Inverse(var mr:TPhysicsMatrix3x3;const ma:TPhysicsMatrix3x3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix3x3Map(const a,b:TPhysicsVector3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix3x3OrthoNormalize(var m:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function Matrix4x4Set(m:TPhysicsMatrix3x3):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4Rotation(m:TPhysicsMatrix4x4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4RotateX(Angle:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4RotateY(Angle:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4RotateZ(Angle:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4Rotate(const Euler:TPhysicsEuler):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix4x4Rotate(Angle:TPhysicsFloat;Axis:TPhysicsVector4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix4x4Translate(x,y,z:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix4x4Translate(const v:TPhysicsVector3):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix4x4Translate(const v:TPhysicsVector4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Matrix4x4Translate(var m:TPhysicsMatrix4x4;const v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure Matrix4x4Translate(var m:TPhysicsMatrix4x4;const v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function Matrix4x4Scale(sx,sy,sz:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix4x4Add(var m1:TPhysicsMatrix4x4;const m2:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix4x4Sub(var m1:TPhysicsMatrix4x4;const m2:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix4x4Mul(var m1:TPhysicsMatrix4x4;const m2:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4TermMul(const m1,m2:TPhysicsMatrix4x4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix4x4ScalarMul(var m:TPhysicsMatrix4x4;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix4x4Transpose(var m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4Determinant(const m:TPhysicsMatrix4x4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4Euler(const m:TPhysicsMatrix4x4):TPhysicsEuler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix4x4SetColumn(var m:TPhysicsMatrix4x4;const c:integer;const v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4GetColumn(const m:TPhysicsMatrix4x4;const c:integer):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure Matrix4x4SetRow(var m:TPhysicsMatrix4x4;const r:integer;const v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4GetRow(const m:TPhysicsMatrix4x4;const r:integer):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4Compare(const m1,m2:TPhysicsMatrix4x4):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4Inverse(var mr:TPhysicsMatrix4x4;const ma:TPhysicsMatrix4x4):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4GetSubMatrix3x3Ex(const m:TPhysicsMatrix4x4;i,j:integer):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4GetSubMatrix3x3(const m:TPhysicsMatrix4x4;i,j:integer):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4Perspective(fovy,Aspect,zNear,zFar:double):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function Matrix4x4LookAt(const Eye,Center,Up:TPhysicsVector3):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function PlaneVectorDistance(const Plane:TPhysicsPlane;const Point:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function PlaneVectorDistance(const Plane:TPhysicsPlane;const Point:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function PlaneFromEdgePoints(P1,P2,N:TPhysicsVector3):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function PlaneFromEdgePoints(P1,P2,N:TPhysicsVector4):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function PlaneFromPoints(P1,P2,P3:TPhysicsVector3):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function PlaneFromPoints(P1,P2,P3:TPhysicsVector4):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;

function QuaternionLengthSquared(const AQuaternion:TPhysicsQuaternion):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function QuaternionNormal(const AQuaternion:TPhysicsQuaternion):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure QuaternionNormalize(var AQuaternion:TPhysicsQuaternion); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function QuaternionFromAxisAngle(const Axis:TPhysicsVector3;Angle:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function QuaternionFromEuler(pitch,yaw,roll:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function QuaternionToMatrix(const AQuaternion:TPhysicsQuaternion):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure QuaternionToAxisAngle(const AQuaternion:TPhysicsQuaternion;var Axis:TPhysicsVector3;var Angle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function QuaternionSlerp(q1,q2:TPhysicsQuaternion;t:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function SphereCoordsFromCartesianVector3(const v:TPhysicsVector3):TPhysicsSphereCoords; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereCoordsFromCartesianVector4(const v:TPhysicsVector4):TPhysicsSphereCoords; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereCoordsToCartesianVector3(const s:TPhysicsSphereCoords):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereCoordsToCartesianVector4(const s:TPhysicsSphereCoords):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function CullSphere(const s:TPhysicsSphere;const p:array of TPhysicsPlane):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereContains(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereContainsEx(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereIntersect(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereIntersectEx(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereRayIntersect(var s:TPhysicsSphere;const Origin,Direction:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereFromAABB(const AABB:TPhysicsAABB):TPhysicsSphere; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SphereExtend(const Sphere,WithSphere:TPhysicsSphere):TPhysicsSphere; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function AABBResize(const AABB:TPhysicsAABB;f:TPhysicsFloat):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function AABBCombine(const AABB,WithAABB:TPhysicsAABB):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function AABBIntersect(const AABB,WithAABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function AABBIntersectEx(const AABB,WithAABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function AABBContains(const InAABB,AABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function AABBContains(const AABB:TPhysicsAABB;Vector:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function AABBContainsEx(const InAABB,AABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function AABBContainsEx(const AABB:TPhysicsAABB;Vector:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function AABBGetIntersectAABB(const AABB,WithAABB:TPhysicsAABB):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function AABBRayIntersect(const AABB:TPhysicsAABB;const Origin,Direction:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function AABBTransform(const DstAABB:TPhysicsAABB;const Transform:TPhysicsMatrix4x4):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function CalculateArea(const v0,v1,v2:TPhysicsVector3):TPhysicsFloat;
function CalculateVolume(const v0,v1,v2,v3:TPhysicsVector3):TPhysicsFloat;

function SegmentTriangleIntersection(var tS,tT0,tT1:TPhysicsFloat;seg:TPhysicsSegment;triangle:TPhysicsSegmentTriangle):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SegmentSegmentDistanceSq(var t0,t1:TPhysicsFloat;seg0,seg1:TPhysicsSegment):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PointTriangleDistanceSq(var pfSParam,pfTParam:TPhysicsFloat;rkPoint:TPhysicsVector3;rkTri:TPhysicsSegmentTriangle):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function SegmentTriangleDistanceSq(var segT,triT0,triT1:TPhysicsFloat;seg:TPhysicsSegment;triangle:TPhysicsSegmentTriangle):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function BoxGetDistanceToPoint(Point:TPhysicsVector3;const Center,Size:TPhysicsVector3;const InvTransformMatrix,TransformMatrix:TPhysicsMatrix4x4;var ClosestBoxPoint:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function GetDistanceFromLine(const s:TPhysicsVector3;var project:TPhysicsVector3;const pointa,pointb:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure LineClosestApproach(const pa,ua,pb,ub:TPhysicsVector3;var Alpha,Beta:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure ClosestLineBoxPoints(const p1,p2,c:TPhysicsVector3;const ir,r:TPhysicsMatrix4x4;const side:TPhysicsVector3;var lret,bret:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure ClosestLineSegmentPoints(const a1,a2,b1,b2:TPhysicsVector3;var cp1,cp2:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsSort(DataBase:pointer;Count:integer;CompareFunction:TPhysicsSortCompareFunction); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsReallocateMemory(var p;Size:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

function PhysicsBoxGetCorners(const Size:TPhysicsVector3):TPhysicsBoxCorners; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsObjectInit(var Instance:TPhysicsObject;ABodyType:integer=BodyMesh;ACollisionBodyType:integer=-1); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectDone(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectClear(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectAddMesh(var Instance:TPhysicsObject):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectMeshAddMesh(var Instance:TPhysicsObjectMesh):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshDone(var Instance:TPhysicsObjectMesh); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshAddTriangle(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsTriangleVertices); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure PhysicsObjectMeshAddTriangle(var Instance:TPhysicsObjectMesh;const v0,v1,v2:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure PhysicsObjectMeshAddTriangles(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsTriangleVertices;Count:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshAddQuad(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsQuadVertices); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure PhysicsObjectMeshAddQuad(var Instance:TPhysicsObjectMesh;const v0,v1,v2,v3:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure PhysicsObjectMeshAddQuads(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsQuadVertices;Count:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshCreateBox(var Instance:TPhysicsObjectMesh;SizeX,SizeY,SizeZ:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshCreateSphere(var Instance:TPhysicsObjectMesh;r:TPhysicsFloat;n:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshCreateCylinder(var Instance:TPhysicsObjectMesh;r,l:TPhysicsFloat;n:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshCreateCapsule(var Instance:TPhysicsObjectMesh;r,l:TPhysicsFloat;n:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshCreatePlane(var Instance:TPhysicsObjectMesh;Plane:TPhysicsPlane); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshSubdivide(var Mesh:TPhysicsObjectMesh;TrianglesMinThreshold:integer=4;MaxDepth:integer=64;Level:integer=0); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshOptimizeToLevelOfDetail(var Mesh:TPhysicsObjectMesh;LOD:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshFinish(var Instance:TPhysicsObjectMesh); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectMeshTranslate(var Instance:TPhysicsObjectMesh;Translation:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectTranslate(var Instance:TPhysicsObject;Translation:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectGenerateConvexHull(var Instance:TPhysicsObject):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectFinish(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectBoxSegmentIntersect(var Instance:TPhysicsObject;var FracOut:TPhysicsFloat;var PosOut,NormalOut:TPhysicsVector3;Segment:TPhysicsSegment):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectHeightMapGetHeight(var Instance:TPhysicsObject;x,y:integer):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectHeightMapGetNormal(var Instance:TPhysicsObject;x,y:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectHeightMapGetHeightNormal(var Instance:TPhysicsObject;x,y:integer;var h:TPhysicsFloat;var n:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectHeightMapGetSurfacePos(var Instance:TPhysicsObject;x,y:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectHeightMapGetSurfacePosAndNormal(var Instance:TPhysicsObject;x,y:integer;var p,n:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectHeightMapGetHeightNormalDistance(var Instance:TPhysicsObject;Point:TPhysicsVector3;var n:TPhysicsVector3):single; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectHeightMapSegmentIntersect(var Instance:TPhysicsObject;var FracValue:TPhysicsFloat;var Pos,Normal:TPhysicsVector3;Segment:TPhysicsSegment):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectSetHeightMap(var Instance:TPhysicsObject;Data:pointer;Width,Height:integer;SizeX,SizeZ:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectUpdateTransform(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectUpdate(var Instance:TPhysicsObject;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectUpdatePosition(var Instance:TPhysicsObject;Position:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectSetRigidBody(var Instance:TPhysicsObject;RigidBody:PPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectSetVector(var Instance:TPhysicsObject;v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsObjectSetMatrix(var Instance:TPhysicsObject;m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectGetMin(var Instance:TPhysicsObject):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectGetMax(var Instance:TPhysicsObject):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectGetCenter(var Instance:TPhysicsObject):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectGetRadius(var Instance:TPhysicsObject):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectRayIntersectionMesh(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectRayIntersectionBox(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectRayIntersectionSphere(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectRayIntersectionCylinder(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectRayIntersectionCapsule(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectRayIntersectionPlane(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsObjectRayIntersection(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3;UserRayProc:TPhysicsObjectRayIntersectionUserProc=nil;UserData:pointer=nil):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsCollideInit(var Instance:TPhysicsCollide); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideDone(var Instance:TPhysicsCollide); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsCollideAddContact(var Instance:TPhysicsCollide;TheObject:PPhysicsObject;const ThePoint,TheNormal:TPhysicsVector3;TheDepth:TPhysicsFloat;SwapObjects:boolean=false;TheMinDepth:boolean=false;DoTransform:boolean=true):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollidePointMesh(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollidePointBox(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollidePointSphere(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollidePointCylinder(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollidePointCapsule(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollidePointPlane(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectMeshMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectBoxMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectBoxBox(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectBoxCylinder(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectBoxPlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectBoxHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectSphereSphere(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectSphereBox(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectSphereMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectSpherePlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectSphereHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCylinderMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCylinderSphere(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCylinderCylinder(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCylinderCapsule(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCylinderPlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCylinderHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCapsuleMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCapsuleSphere(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCapsuleBox(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCapsuleCapsule(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCapsulePlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCapsuleHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectPlaneMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectHeightMapMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectConvexHullMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectBoxConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectSphereConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCylinderConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectCapsuleConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectPlaneConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectHeightMapConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideObjectConvexHullConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideFinishObjectMesh(var Instance:TPhysicsCollide;var TheObjectMesh:TPhysicsObjectMesh;Transform:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideFinishObject(var Instance:TPhysicsCollide;TheObject:PPhysicsObject;Transform:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsCollideSort(var Instance:TPhysicsCollide); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsCollideGetContactCount(var Instance:TPhysicsCollide):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsCollideGetContact(var Instance:TPhysicsCollide;Number:integer;var Point,Normal:TPhysicsVector3;var Depth:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsCollide(var Instance:TPhysicsCollide;TheObject:PPhysicsObject):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function PhysicsCollidePoint(var Instance:TPhysicsCollide;Point:TPhysicsVector3;Radius:TPhysicsFloat):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;

procedure PhysicsJointInit(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointDone(var Instance:TPhysicsJoint); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointSetActive(var Instance:TPhysicsJoint;Active:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsJointIsActive(var Instance:TPhysicsJoint):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointSetDestroyMinVelocity(var Instance:TPhysicsJoint;Active:boolean;DestroyMinVelocity:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsJointRestrictionResponse(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat;const Point0,Point1:TPhysicsVector3;MinDist:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointInitBase(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointInitBall(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointInitHinge(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,Axis,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointInitUniversal(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,Axis0,Axis1,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointInitUser(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;UserResponseProc:TPhysicsJointUserResponseProc;Data:pointer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointHingeSetAxis(var Instance:TPhysicsJoint;Number:integer;const Axis:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsJointHingeSetAngularVelocity(var Instance:TPhysicsJoint;Number:integer;Velocity:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsConstraintInit(var Instance:TPhysicsConstraint;RigidBody1:PPhysicsRigidBody;RigidBody2:PPhysicsRigidBody=nil); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsConstraintInitMaxDistance(var Instance:TPhysicsConstraint;RigidBody1,RigidBody2:PPhysicsRigidBody;MaxDistance:TPhysicsFloat;MaxDistanceBodyPos0,MaxDistanceBodyPos1:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsConstraintInitPoint(var Instance:TPhysicsConstraint;RigidBody1,RigidBody2:PPhysicsRigidBody;PointAllowedDistance,PointTimeScale:TPhysicsFloat;PointBodyPos0,PointBodyPos1:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsConstraintInitWorldPoint(var Instance:TPhysicsConstraint;RigidBody:PPhysicsRigidBody;WorldPointPointOnBody,WorldPoint:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsConstraintInitVelocity(var Instance:TPhysicsConstraint;RigidBody:PPhysicsRigidBody;Velocity,VelocityAngular:TPhysicsVector3;VelocityDo,VelocityDoAngular:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsConstraintInitUser(var Instance:TPhysicsConstraint;RigidBody:PPhysicsRigidBody;UserResponseProc:TPhysicsConstraintUserResponseProc;Data:pointer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsConstraintDone(var Instance:TPhysicsConstraint); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsConstraintSetActive(var Instance:TPhysicsConstraint;Active:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsConstraintIsActive(var Instance:TPhysicsConstraint):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsRigidBodyInit(var Instance:TPhysicsRigidBody;AObject:PPhysicsObject;AMass,ARestitution,AFriction:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyDone(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyCalculateMeshVolume(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyCalculateMeshCenterOfMass(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyCalculateMeshInertiaTensor(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodySetMatrix(var Instance:TPhysicsRigidBody;m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodySetVector(var Instance:TPhysicsRigidBody;v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyLimit(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodySetAngularMomentum(var Instance:TPhysicsRigidBody;const AngularMomentum:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsRigidBodyGetAngularMomentum(var Instance:TPhysicsRigidBody):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyAddImpulse(var Instance:TPhysicsRigidBody;const Point,Impulse:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure PhysicsRigidBodyAddImpulse(var Instance:TPhysicsRigidBody;const Impulse:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
procedure PhysicsRigidBodyDamp(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyCalcForce(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyIntegrateVelocity(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyIntegratePos(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsRigidBodyFindContacts(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsRigidBodyContactsResponse(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat;ZeroRestitution:boolean=false):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsSweepAndPruneObjectAddPair(var Instance:TPhysics;ObjectA,ObjectB:PPhysicsObject;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsSweepAndPruneAddPair(var Instance:TPhysics;ObjectA,ObjectB:PPhysicsObject;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsSweepAndPruneAxisSort(var Instance:TPhysics;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsSweepAndPruneAxisSearch(var Instance:TPhysics;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsSweepAndPruneAxisUpdate(var Instance:TPhysics;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsSweepAndPruneUpdate(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

procedure PhysicsInit(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsDone(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsUpdateOldObjectValues(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
procedure PhysicsUpdate(var Instance:TPhysics;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
function PhysicsRayIntersection(var Instance:TPhysics;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3;UserRayProc:TPhysicsObjectRayIntersectionUserProc=nil;UserData:pointer=nil):PPhysicsObject; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}

{$ifdef win32}
function PhysicsObjectLoadMesh(var DstObject:TPhysicsObject;SrcData:pointer;SrcSize:longword):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
function PhysicsObjectLoadMesh(var DstObject:TPhysicsObject;SrcFileName:pchar):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
{$else}
function PhysicsObjectLoadMesh(var DstObject:TPhysicsObject;SrcData:pointer;SrcSize:longword):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
{$endif}

implementation

{$ifdef win32}
uses Windows;
{$endif}

type pbyte=^byte;

{$ifdef physicsdouble}
type PFLOAT64=^TFLOAT64;
     TFLOAT64=packed record
{$ifdef LITTLE_ENDIAN}
      Lo:longword;
      Hi:longword;
{$else}
      Hi:longword;
      Lo:longword;
{$endif}
     end;

function SoftTRUNC(FloatValue:double):integer; {$ifdef caninline}inline;{$endif}
var Exponent,Sig0,Sig1,SigExtra,Signed,IsDenormalized:longword;
    Value,Shift:integer;
    Float64:TFLOAT64 absolute FloatValue;
begin
 Sig0:=Float64.Hi and $000fffff;
 Sig1:=Float64.Lo;
 Exponent:=(Float64.Hi shr 20) and $7ff;
 Shift:=Exponent-$413;
 Sig0:=Sig0 or $100000;
 SigExtra:=((Sig0 shl Shift) or (Sig1 shr ((32-Shift) and 31)) and (-ord(Shift<0))) or (Sig0 and (-ord(Shift=0)));
 IsDenormalized:=-ord(0<=Shift);
 Value:=(((-ord(Exponent>=$3ff)) and (Sig0 shr (32-Shift))) and not IsDenormalized) or
        (SigExtra and IsDenormalized);
 Signed:=-ord((Float64.Hi and $80000000)<>0);
 result:=(((-Value) and Signed) or (Value and not Signed));
end;
{$else}
function SoftTRUNC(FloatValue:single):integer; {$ifdef caninline}inline;{$endif}
const MaskMantissa=(1 shl 23)-1;
var Exponent,Mantissa,Sig,SigExtra,Signed,IsDenormalized:longword;
    Value,Shift:integer;
begin
 Exponent:=(plongword(@FloatValue)^ and $7fffffff) shr 23;
 Mantissa:=plongword(@FloatValue)^and MaskMantissa;
 Shift:=Exponent-$96;
 Sig:=Mantissa or $00800000;
 SigExtra:=Sig shl (Shift and 31);
 IsDenormalized:=0-ord(0<=Shift);
 Value:=(((-ord(Exponent>=$7e)) and (Sig shr (32-Shift))) and not IsDenormalized) or
        (SigExtra and IsDenormalized);
 Signed:=-ord((plongword(@FloatValue)^ and $80000000)<>0);
 result:=(((-Value) and Signed) or (Value and not Signed)) and (0-ord($9e>Exponent));
end;
{$endif}

function GetSign(x:TPhysicsFloat):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if x<-EPSILON then begin
  result:=-1;
 end else if x>EPSILON then begin
  result:=1;
 end else begin
  result:=0;
 end;
end;

function GetSignEx(x:TPhysicsFloat):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if x<0 then begin
  result:=-1;
 end else begin
  result:=1;
 end;
end;

function Min(a,b:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function MinEx(a,b:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Max(a,b:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if a>b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function MaxEx(a,b:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if a>b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function MinInt(a,b:integer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function MaxInt(a,b:integer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if a>b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Tan(Angle:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sin(Angle)/cos(Angle);
end;

{$ifndef CPU64}
function Frac(x:TPhysicsFloat):TPhysicsFloat; assembler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
asm
 fld x
 fld1
 fxch st(1)
 fprem
 fxch st(1)
 fstp st(0)
end;

function Power(number,exponent:TPhysicsFloat):TPhysicsFloat; assembler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
asm
 fld exponent
 fld number
 fyl2x
 fld1
 fld st(1)
 fprem
 f2xm1
 faddp st(1),st
 fscale
 fstp st(1)
end;

function ArcTan2(y,x:TPhysicsFloat):TPhysicsFloat; assembler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
asm
 fld y
 fld x
 fpatan
 fwait
end;
{$else}
function Frac(x:TPhysicsFloat):TPhysicsFloat; assembler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=System.frac(x);//x-SoftTRUNC(x);
end;

function Power(number,exponent:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=math.power(number,exponent);
end;

function ArcTan2(y,x:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=math.arctan2(y,x);
end;
{$endif}

function ArcCos(x:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=ArcTan2(sqrt(1-(x*x)),x);
end;

function ArcSin(x:TPhysicsFloat):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=ArcTan2(x,sqrt(1-(x*x)));
end;

function Vector2(x,y:TPhysicsFloat):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=x;
 result.y:=y;
end;

function Vector3(x,y,z:TPhysicsFloat):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
end;

function Vector3(v:TPhysicsVector4):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.x:=v.x;
 result.y:=v.y;
 result.z:=v.z;
end;

function Vector4(x,y,z:TPhysicsFloat;w:TPhysicsFloat=1):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.x:=x;
 result.y:=y;
 result.z:=z;
 result.w:=w;
end;

function Vector4(v:TPhysicsVector3;w:TPhysicsFloat=1):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.x:=v.x;
 result.y:=v.y;
 result.z:=v.z;
 result.w:=w;
end;

function Matrix3x3(const xx,xy,xz,yx,yy,yz,zx,zy,zz:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result[0,0]:=xx;
 result[0,1]:=yx;
 result[0,2]:=zx;
 result[1,0]:=xy;
 result[1,1]:=yy;
 result[1,2]:=zy;
 result[2,0]:=xz;
 result[2,1]:=yz;
 result[2,2]:=zz;
end;

function Matrix3x3(const x,y,z:TPhysicsVector3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result[0,0]:=x.x;
 result[0,1]:=x.y;
 result[0,2]:=x.z;
 result[1,0]:=y.x;
 result[1,1]:=y.y;
 result[1,2]:=y.z;
 result[2,0]:=z.x;
 result[2,1]:=z.y;
 result[2,2]:=z.z;
end;

function Matrix3x3Raw(const xx,xy,xz,yx,yy,yz,zx,zy,zz:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=xx;
 result[0,1]:=xy;
 result[0,2]:=xz;
 result[1,0]:=yx;
 result[1,1]:=yy;
 result[1,2]:=yz;
 result[2,0]:=zx;
 result[2,1]:=zy;
 result[2,2]:=zz;
end;

function Matrix4x4(const xx,xy,xz,xw,yx,yy,yz,yw,zx,zy,zz,zw,wx,wy,wz,ww:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result[0,0]:=xx;
 result[0,1]:=yx;
 result[0,2]:=zx;
 result[0,3]:=wx;
 result[1,0]:=xy;
 result[1,1]:=yy;
 result[1,2]:=zy;
 result[1,3]:=wy;
 result[2,0]:=xz;
 result[2,1]:=yz;
 result[2,2]:=zz;
 result[2,3]:=wz;
 result[3,0]:=xw;
 result[3,1]:=yw;
 result[3,2]:=zw;
 result[3,3]:=ww;
end;

function Matrix4x4(const x,y,z,w:TPhysicsVector4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result[0,0]:=x.x;
 result[0,1]:=x.y;
 result[0,2]:=x.z;
 result[0,3]:=x.w;
 result[1,0]:=y.x;
 result[1,1]:=y.y;
 result[1,2]:=y.z;
 result[1,3]:=y.w;
 result[2,0]:=z.x;
 result[2,1]:=z.y;
 result[2,2]:=z.z;
 result[2,3]:=z.w;
 result[3,0]:=w.x;
 result[3,1]:=w.y;
 result[3,2]:=w.z;
 result[3,3]:=w.w;
end;

function Matrix4x4Raw(const xx,xy,xz,xw,yx,yy,yz,yw,zx,zy,zz,zw,wx,wy,wz,ww:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=xx;
 result[0,1]:=xy;
 result[0,2]:=xz;
 result[0,3]:=xw;
 result[1,0]:=yx;
 result[1,1]:=yy;
 result[1,2]:=yz;
 result[1,3]:=yw;
 result[2,0]:=zx;
 result[2,1]:=zy;
 result[2,2]:=zz;
 result[2,3]:=zw;
 result[3,0]:=wx;
 result[3,1]:=wy;
 result[3,2]:=wz;
 result[3,3]:=ww;
end;

function Plane(a,b,c,d:TPhysicsFloat):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.a:=a;
 result.b:=b;
 result.c:=c;
 result.d:=d;
end;

function Plane(Normal:TPhysicsVector3;Distance:TPhysicsFloat):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.Normal:=Normal;
 result.Distance:=Distance;
end;

function Quaternion(w,x,y,z:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.w:=w;
 result.x:=x;
 result.y:=y;
 result.z:=z;
end;

function Euler(Pitch,Yaw,Roll:TPhysicsFloat):TPhysicsEuler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.Pitch:=Pitch;
 result.Yaw:=Yaw;
 result.Roll:=Roll;
end;

function Vector2Compare(const v1,v2:TPhysicsVector2):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=(abs(v1.x-v2.x)<EPSILON) and (abs(v1.y-v2.y)<EPSILON);
end;

function Vector2Add(const v1,v2:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x+v2.x;
 result.y:=v1.y+v2.y;
end;

function Vector2Sub(const v1,v2:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x-v2.x;
 result.y:=v1.y-v2.y;
end;

function Vector2ScalarMul(const v:TPhysicsVector2;s:TPhysicsFloat):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v.x*s;
 result.y:=v.y*s;
end;

function Vector2Dot(const v1,v2:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=v1.x*v2.x+v1.y*v2.y;
end;

function Vector2Neg(const v:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=-v.x;
 result.y:=-v.y;
end;

procedure Vector2Scale(var v:TPhysicsVector2;sx,sy:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 v.x:=v.x*sx;
 v.y:=v.y*sy;
end;

procedure Vector2Scale(var v:TPhysicsVector2;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 v.x:=v.x*s;
 v.y:=v.y*s;
end;

function Vector2Mul(const v1,v2:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x*v2.x;
 result.y:=v1.y*v2.y;
end;

function Vector2Length(const v:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sqrt(sqr(v.x)+sqr(v.y));
end;

function Vector2Dist(const v1,v2:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Vector2Length(Vector2Sub(v2,v1));
end;

function Vector2LengthSquared(const v:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sqr(v.x)+sqr(v.y);
end;

function Vector2Angle(const v1,v2,v3:TPhysicsVector2):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var a1,a2:TPhysicsVector2;
    l1,l2:TPhysicsFloat;
begin
 a1:=Vector2Sub(v1,v2);
 a2:=Vector2Sub(v3,v2);
 l1:=Vector2Length(a1);
 l2:=Vector2Length(a2);
 if (l1=0) or (l2=0) then begin
  result:=0;
 end else begin
  result:=ArcCos(Vector2Dot(a1,a2)/(l1*l2));
 end;
end;

procedure Vector2Normalize(var v:TPhysicsVector2); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var l:TPhysicsFloat;
begin
 l:=Vector2Length(v);
 if l=0 then begin
  v:=Vector2Origin;
 end else begin
  Vector2Scale(v,1/l);
 end;
end;

function Vector2Norm(const v:TPhysicsVector2):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var l:TPhysicsFloat;
begin
 l:=Vector2Length(v);
 if l=0 then begin
  result:=Vector2Origin;
 end else begin
  result:=Vector2ScalarMul(v,1/l);
 end;
end;

procedure Vector2Rotate(var v:TPhysicsVector2;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var r:TPhysicsVector2;
begin
 r.x:=(v.x*cos(a))-(v.y*sin(a));
 r.y:=(v.y*cos(a))+(v.x*sin(a));
 v:=r;
end;

procedure Vector2Rotate(var v:TPhysicsVector2;const Center:TPhysicsVector2;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var v0,r:TPhysicsVector2;
begin
 v0:=Vector2Sub(v,Center);
 r.x:=(v0.x*cos(a))-(v0.y*sin(a));
 r.y:=(v0.y*cos(a))+(v0.x*sin(a));
 v:=Vector2Add(r,Center);
end;

procedure Vector2MatrixMul(var v:TPhysicsVector2;const m:TPhysicsMatrix2x2); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector2;
begin
 t.x:=(m[0,0]*v.x)+(m[1,0]*v.y);
 t.y:=(m[0,1]*v.x)+(m[1,1]*v.y);
 v:=t;
end;

function Vector2Lerp(const v1,v2:TPhysicsVector2;w:TPhysicsFloat):TPhysicsVector2; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=((1-w)*v1.x)+(w*v2.x);
 result.y:=((1-w)*v1.y)+(w*v2.y);
end;

function Vector3Compare(const v1,v2:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=(abs(v1.x-v2.x)<EPSILON) and (abs(v1.y-v2.y)<EPSILON) and (abs(v1.z-v2.z)<EPSILON);
end;

function Vector3Add(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x+v2.x;
 result.y:=v1.y+v2.y;
 result.z:=v1.z+v2.z;
end;

function Vector3Sub(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x-v2.x;
 result.y:=v1.y-v2.y;
 result.z:=v1.z-v2.z;
end;

function Vector3ScalarMul(const v:TPhysicsVector3;s:TPhysicsFloat):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v.x*s;
 result.y:=v.y*s;
 result.z:=v.z*s;
end;

function Vector3Dot(const v1,v2:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=(v1.x*v2.x)+(v1.y*v2.y)+(v1.z*v2.z);
end;

function Vector3Cos(const v1,v2:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var d:extended;
begin
 d:=sqrt(Vector3LengthSquared(v1)*Vector3LengthSquared(v2));
 if abs(d)>EPSILON then begin
  result:=((v1.x*v2.x)+(v1.y*v2.y)+(v1.z*v2.z))/d;
  //result:=Vector3Dot(v1,v2)/d;
 end else begin
  result:=0;
 end;
end;

function Vector3Cross(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=(v1.y*v2.z)-(v1.z*v2.y);
 result.y:=(v1.z*v2.x)-(v1.x*v2.z);
 result.z:=(v1.x*v2.y)-(v1.y*v2.x);
end;

function Vector3Neg(const v:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=-v.x;
 result.y:=-v.y;
 result.z:=-v.z;
end;

procedure Vector3Scale(var v:TPhysicsVector3;sx,sy,sz:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 v.x:=v.x*sx;
 v.y:=v.y*sy;
 v.z:=v.z*sz;
end;

procedure Vector3Scale(var v:TPhysicsVector3;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 v.x:=v.x*s;
 v.y:=v.y*s;
 v.z:=v.z*s;
end;

function Vector3Mul(const v1,v2:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x*v2.x;
 result.y:=v1.y*v2.y;
 result.z:=v1.z*v2.z;
end;

function Vector3Length(const v:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sqrt(sqr(v.x)+sqr(v.y)+sqr(v.z));
end;

function Vector3Dist(const v1,v2:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Vector3Length(Vector3Sub(v2,v1));
end;

function Vector3LengthSquared(const v:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sqr(v.x)+sqr(v.y)+sqr(v.z);
end;

function Vector3Angle(const v1,v2,v3:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var a1,a2:TPhysicsVector3;
    l1,l2:TPhysicsFloat;
begin
 a1:=Vector3Sub(v1,v2);
 a2:=Vector3Sub(v3,v2);
 l1:=Vector3Length(a1);
 l2:=Vector3Length(a2);
 if (abs(l1)<EPSILON) or (abs(l2)<EPSILON) then begin
  result:=0;
 end else begin
  result:=ArcCos(Vector3Dot(a1,a2)/(l1*l2));
 end;
end;

procedure Vector3Normalize(var v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var l:TPhysicsFloat;
begin
 l:=Vector3Length(v);
 if abs(l)<EPSILON then begin
  v:=Vector3Origin;
 end else begin
  Vector3Scale(v,1/l);
 end;
end;

function Vector3Norm(const v:TPhysicsVector3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var l:TPhysicsFloat;
begin
 l:=Vector3Length(v);
 if abs(l)<EPSILON then begin
  result:=Vector3Origin;
 end else begin
  result:=Vector3ScalarMul(v,1/l);
 end;
end;

procedure Vector3RotateX(var v:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector3;
begin
 t.x:=v.x;
 t.y:=(v.y*cos(a))+(v.z*-sin(a));
 t.z:=(v.y*sin(a))+(v.z*cos(a));
 v:=t;
end;

procedure Vector3RotateY(var v:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector3;
begin
 t.x:=(v.x*cos(a))+(v.z*sin(a));
 t.y:=v.y;
 t.z:=(v.x*-sin(a))+(v.z*cos(a));
 v:=t;
end;

procedure Vector3RotateZ(var v:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector3;
begin
 t.x:=(v.x*cos(a))+(v.y*-sin(a));
 t.y:=(v.x*sin(a))+(v.y*cos(a));
 t.z:=v.z;
 v:=t;
end;

procedure Vector3MatrixMul(var v:TPhysicsVector3;const m:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var t:TPhysicsVector3;
begin
 t.x:=(m[0,0]*v.x)+(m[1,0]*v.y)+(m[2,0]*v.z);
 t.y:=(m[0,1]*v.x)+(m[1,1]*v.y)+(m[2,1]*v.z);
 t.z:=(m[0,2]*v.x)+(m[1,2]*v.y)+(m[2,2]*v.z);
 v:=t;
end;

procedure Vector3MatrixMul(var v:TPhysicsVector3;const m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var t:TPhysicsVector3;
begin
 t.x:=(m[0,0]*v.x)+(m[1,0]*v.y)+(m[2,0]*v.z)+m[3,0];
 t.y:=(m[0,1]*v.x)+(m[1,1]*v.y)+(m[2,1]*v.z)+m[3,1];
 t.z:=(m[0,2]*v.x)+(m[1,2]*v.y)+(m[2,2]*v.z)+m[3,2];
 v:=t;
end;

procedure Vector3NormalMatrixMul(var v:TPhysicsVector3;const m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var t:TPhysicsVector3;
begin
 t.x:=(m[0,0]*v.x)+(m[1,0]*v.y)+(m[2,0]*v.z);
 t.y:=(m[0,1]*v.x)+(m[1,1]*v.y)+(m[2,1]*v.z);
 t.z:=(m[0,2]*v.x)+(m[1,2]*v.y)+(m[2,2]*v.z);
 v:=t;
end;

function Vector3TermMatrixMul(const v:TPhysicsVector3;const m:TPhysicsMatrix3x3):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.x:=(m[0,0]*v.x)+(m[1,0]*v.y)+(m[2,0]*v.z);
 result.y:=(m[0,1]*v.x)+(m[1,1]*v.y)+(m[2,1]*v.z);
 result.z:=(m[0,2]*v.x)+(m[1,2]*v.y)+(m[2,2]*v.z);
end;

function Vector3TermMatrixMul(const v:TPhysicsVector3;const m:TPhysicsMatrix4x4):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result.x:=(m[0,0]*v.x)+(m[1,0]*v.y)+(m[2,0]*v.z)+m[3,0];
 result.y:=(m[0,1]*v.x)+(m[1,1]*v.y)+(m[2,1]*v.z)+m[3,1];
 result.z:=(m[0,2]*v.x)+(m[1,2]*v.y)+(m[2,2]*v.z)+m[3,2];
end;

procedure Vector3Rotate(var v:TPhysicsVector3;const Axis:TPhysicsVector3;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Vector3MatrixMul(v,Matrix3x3Rotate(a,Axis));
end;

function Vector3Lerp(const v1,v2:TPhysicsVector3;w:TPhysicsFloat):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=((1-w)*v1.x)+(w*v2.x);
 result.y:=((1-w)*v1.y)+(w*v2.y);
 result.z:=((1-w)*v1.z)+(w*v2.z);
end;

function Vector4Compare(const v1,v2:TPhysicsVector4):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=(abs(v1.x-v2.x)<EPSILON) and (abs(v1.y-v2.y)<EPSILON) and (abs(v1.z-v2.z)<EPSILON) and (abs(v1.w-v2.w)<EPSILON);
end;

function Vector4Add(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x+v2.x;
 result.y:=v1.y+v2.y;
 result.z:=v1.z+v2.z;
 result.w:=1;
end;

function Vector4Sub(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x-v2.x;
 result.y:=v1.y-v2.y;
 result.z:=v1.z-v2.z;
 result.w:=1;
end;

function Vector4ScalarMul(const v:TPhysicsVector4;s:TPhysicsFloat):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v.x*s;
 result.y:=v.y*s;
 result.z:=v.z*s;
 result.w:=1;
end;

function Vector4Dot(const v1,v2:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=(v1.x*v2.x)+(v1.y*v2.y)+(v1.z*v2.z)+(v1.w*v2.w);
end;

function Vector4Cross(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=(v1.y*v2.z)-(v2.y*v1.z);
 result.y:=(v2.x*v1.z)-(v1.x*v2.z);
 result.z:=(v1.x*v2.y)-(v2.x*v1.y);
 result.w:=1;
end;

function Vector4Neg(const v:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=-v.x;
 result.y:=-v.y;
 result.z:=-v.z;
 result.w:=1;
end;

procedure Vector4Scale(var v:TPhysicsVector4;sx,sy,sz:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 v.x:=v.x*sx;
 v.y:=v.y*sy;
 v.z:=v.z*sz;
end;

procedure Vector4Scale(var v:TPhysicsVector4;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 v.x:=v.x*s;
 v.y:=v.y*s;
 v.z:=v.z*s;
end;

function Vector4Mul(const v1,v2:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=v1.x*v2.x;
 result.y:=v1.y*v2.y;
 result.z:=v1.z*v2.z;
 result.w:=1;
end;

function Vector4Length(const v:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sqrt(v.x*v.x+v.y*v.y+v.z*v.z);
end;

function Vector4Dist(const v1,v2:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Vector4Length(Vector4Sub(v2,v1));
end;

function Vector4LengthSquared(const v:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=v.x*v.x+v.y*v.y+v.z*v.z;
end;

function Vector4Angle(const v1,v2,v3:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var a1,a2:TPhysicsVector4;
    l1,l2:TPhysicsFloat;
begin
 a1:=Vector4Sub(v1,v2);
 a2:=Vector4Sub(v3,v2);
 l1:=Vector4Length(a1);
 l2:=Vector4Length(a2);
 if (l1=0) or (l2=0) then begin
  result:=0;
 end else begin
  result:=ArcCos(Vector4Dot(a1,a2)/(l1*l2));
 end;
end;

procedure Vector4Normalize(var v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var l:TPhysicsFloat;
begin
 l:=Vector4Length(v);
 if l=0 then begin
  v:=Vector4Origin;
 end else begin
  Vector4Scale(v,1/l);
 end;
end;

function Vector4Norm(const v:TPhysicsVector4):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var l:TPhysicsFloat;
begin
 l:=Vector4Length(v);
 if l=0 then begin
  result:=Vector4Origin;
 end else begin
  result:=Vector4ScalarMul(v,1/l);
 end;
end;

procedure Vector4RotateX(var v:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector4;
begin
 t.x:=v.x;
 t.y:=(v.y*cos(a))+(v.z*-sin(a));
 t.z:=(v.y*sin(a))+(v.z*cos(a));
 t.w:=1;
 v:=t;
end;

procedure Vector4RotateY(var v:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector4;
begin
 t.x:=(v.x*cos(a))+(v.z*sin(a));
 t.y:=v.y;
 t.z:=(v.x*-sin(a))+(v.z*cos(a));
 t.w:=1;
 v:=t;
end;

procedure Vector4RotateZ(var v:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector4;
begin
 t.x:=(v.x*cos(a))+(v.y*-sin(a));
 t.y:=(v.x*sin(a))+(v.y*cos(a));
 t.z:=v.z;
 t.w:=1;
 v:=t;
end;

procedure Vector4MatrixMul(var v:TPhysicsVector4;const m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsVector4;
begin
 t.x:=(m[0,0]*v.x)+(m[1,0]*v.y)+(m[2,0]*v.z)+(m[3,0]*v.w);
 t.y:=(m[0,1]*v.x)+(m[1,1]*v.y)+(m[2,1]*v.z)+(m[3,1]*v.w);
 t.z:=(m[0,2]*v.x)+(m[1,2]*v.y)+(m[2,2]*v.z)+(m[3,2]*v.w);
 t.w:=(m[0,3]*v.x)+(m[1,3]*v.y)+(m[2,3]*v.z)+(m[3,3]*v.w);
 v:=t;
end;

procedure Vector4Rotate(var v:TPhysicsVector4;const Axis:TPhysicsVector4;a:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Vector4MatrixMul(v,Matrix4x4Rotate(a,Axis));
end;

function Vector4Lerp(const v1,v2:TPhysicsVector4;w:TPhysicsFloat):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=(1-w)*v1.x+w*v2.x;
 result.y:=(1-w)*v1.y+w*v2.y;
 result.z:=(1-w)*v1.z+w*v2.z;
 result.w:=1;
end;

function Matrix3x3RotateX(Angle:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix3x3Identity;
 result[1,1]:=cos(Angle);
 result[2,2]:=result[1,1];
 result[1,2]:=sin(Angle);
 result[2,1]:=-result[1,2];
end;

function Matrix3x3RotateY(Angle:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix3x3Identity;
 result[0,0]:=cos(Angle);
 result[2,2]:=result[0,0];
 result[0,2]:=-sin(Angle);
 result[2,0]:=-result[0,2];
end;

function Matrix3x3RotateZ(Angle:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix3x3Identity;
 result[0,0]:=cos(Angle);
 result[1,1]:=result[0,0];
 result[0,1]:=sin(Angle);
 result[1,0]:=-result[0,1];
end;

function Matrix3x3Rotate(Euler:TPhysicsEuler):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var a,b,c,d,e,f,ad,bd:TPhysicsFloat;
begin
 a:=cos(Euler.Pitch);
 b:=sin(Euler.Pitch);
 c:=cos(Euler.Yaw);
 d:=sin(Euler.Yaw);
 e:=cos(Euler.Roll);
 f:=sin(Euler.Roll);
 ad:=a*d;
 bd:=b*d;
 result:=Matrix3x3Identity;
 result[0,0]:=c*e;
 result[1,0]:=-c*f;
 result[2,0]:=d;
 result[0,1]:=bd*e+a*f;
 result[1,1]:=-bd*f+a*e;
 result[2,1]:=-b*c;
 result[0,2]:=-ad*e+b*f;
 result[1,2]:=ad*f+b*e;
 result[2,2]:=a*c;
end;

function Matrix3x3Rotate(Angle:TPhysicsFloat;Axis:TPhysicsVector3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var m:TPhysicsMatrix3x3;
    CosinusAngle,SinusAngle:TPhysicsFloat;
begin
 m:=Matrix3x3Identity;
 CosinusAngle:=cos(Angle);
 SinusAngle:=sin(Angle);
 m[0,0]:=CosinusAngle+(1-CosinusAngle)*Axis.x*Axis.x;
 m[1,0]:=(1-CosinusAngle)*Axis.x*Axis.y-Axis.z*SinusAngle;
 m[2,0]:=(1-CosinusAngle)*Axis.x*Axis.z+Axis.y*SinusAngle;
 m[0,1]:=(1-CosinusAngle)*Axis.x*Axis.z+Axis.z*SinusAngle;
 m[1,1]:=CosinusAngle+(1-CosinusAngle)*Axis.y*Axis.y;
 m[2,1]:=(1-CosinusAngle)*Axis.y*Axis.z-Axis.x*SinusAngle;
 m[0,2]:=(1-CosinusAngle)*Axis.x*Axis.z-Axis.y*SinusAngle;
 m[1,2]:=(1-CosinusAngle)*Axis.y*Axis.z+Axis.x*SinusAngle;
 m[2,2]:=CosinusAngle+(1-CosinusAngle)*Axis.z*Axis.z;
 result:=m;
end;

function Matrix3x3Scale(sx,sy,sz:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix3x3Identity;
 result[0,0]:=sx;
 result[1,1]:=sy;
 result[2,2]:=sz;
end;

procedure Matrix3x3Add(var m1:TPhysicsMatrix3x3;const m2:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m1[0,0]:=m1[0,0]+m2[0,0];
 m1[0,1]:=m1[0,1]+m2[0,1];
 m1[0,2]:=m1[0,2]+m2[0,2];
 m1[1,0]:=m1[1,0]+m2[1,0];
 m1[1,1]:=m1[1,1]+m2[1,1];
 m1[1,2]:=m1[1,2]+m2[1,2];
 m1[2,0]:=m1[2,0]+m2[2,0];
 m1[2,1]:=m1[2,1]+m2[2,1];
 m1[2,2]:=m1[2,2]+m2[2,2];
end;

procedure Matrix3x3Sub(var m1:TPhysicsMatrix3x3;const m2:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m1[0,0]:=m1[0,0]-m2[0,0];
 m1[0,1]:=m1[0,1]-m2[0,1];
 m1[0,2]:=m1[0,2]-m2[0,2];
 m1[1,0]:=m1[1,0]-m2[1,0];
 m1[1,1]:=m1[1,1]-m2[1,1];
 m1[1,2]:=m1[1,2]-m2[1,2];
 m1[2,0]:=m1[2,0]-m2[2,0];
 m1[2,1]:=m1[2,1]-m2[2,1];
 m1[2,2]:=m1[2,2]-m2[2,2];
end;

procedure Matrix3x3Mul(var m1:TPhysicsMatrix3x3;const m2:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsMatrix3x3;
begin
 t[0,0]:=(m1[0,0]*m2[0,0])+(m1[0,1]*m2[1,0])+(m1[0,2]*m2[2,0]);
 t[0,1]:=(m1[0,0]*m2[0,1])+(m1[0,1]*m2[1,1])+(m1[0,2]*m2[2,1]);
 t[0,2]:=(m1[0,0]*m2[0,2])+(m1[0,1]*m2[1,2])+(m1[0,2]*m2[2,2]);
 t[1,0]:=(m1[1,0]*m2[0,0])+(m1[1,1]*m2[1,0])+(m1[1,2]*m2[2,0]);
 t[1,1]:=(m1[1,0]*m2[0,1])+(m1[1,1]*m2[1,1])+(m1[1,2]*m2[2,1]);
 t[1,2]:=(m1[1,0]*m2[0,2])+(m1[1,1]*m2[1,2])+(m1[1,2]*m2[2,2]);
 t[2,0]:=(m1[2,0]*m2[0,0])+(m1[2,1]*m2[1,0])+(m1[2,2]*m2[2,0]);
 t[2,1]:=(m1[2,0]*m2[0,1])+(m1[2,1]*m2[1,1])+(m1[2,2]*m2[2,1]);
 t[2,2]:=(m1[2,0]*m2[0,2])+(m1[2,1]*m2[1,2])+(m1[2,2]*m2[2,2]);
 m1:=t;
end;

function Matrix3x3TermAdd(const m1,m2:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=m1[0,0]+m2[0,0];
 result[0,1]:=m1[0,1]+m2[0,1];
 result[0,2]:=m1[0,2]+m2[0,2];
 result[1,0]:=m1[1,0]+m2[1,0];
 result[1,1]:=m1[1,1]+m2[1,1];
 result[1,2]:=m1[1,2]+m2[1,2];
 result[2,0]:=m1[2,0]+m2[2,0];
 result[2,1]:=m1[2,1]+m2[2,1];
 result[2,2]:=m1[2,2]+m2[2,2];
end;

function Matrix3x3TermSub(const m1,m2:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=m1[0,0]-m2[0,0];
 result[0,1]:=m1[0,1]-m2[0,1];
 result[0,2]:=m1[0,2]-m2[0,2];
 result[1,0]:=m1[1,0]-m2[1,0];
 result[1,1]:=m1[1,1]-m2[1,1];
 result[1,2]:=m1[1,2]-m2[1,2];
 result[2,0]:=m1[2,0]-m2[2,0];
 result[2,1]:=m1[2,1]-m2[2,1];
 result[2,2]:=m1[2,2]-m2[2,2];
end;

function Matrix3x3TermMul(const m1,m2:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=(m2[0,0]*m1[0,0])+(m2[0,1]*m1[1,0])+(m2[0,2]*m1[2,0]);
 result[0,1]:=(m2[0,0]*m1[0,1])+(m2[0,1]*m1[1,1])+(m2[0,2]*m1[2,1]);
 result[0,2]:=(m2[0,0]*m1[0,2])+(m2[0,1]*m1[1,2])+(m2[0,2]*m1[2,2]);
 result[1,0]:=(m2[1,0]*m1[0,0])+(m2[1,1]*m1[1,0])+(m2[1,2]*m1[2,0]);
 result[1,1]:=(m2[1,0]*m1[0,1])+(m2[1,1]*m1[1,1])+(m2[1,2]*m1[2,1]);
 result[1,2]:=(m2[1,0]*m1[0,2])+(m2[1,1]*m1[1,2])+(m2[1,2]*m1[2,2]);
 result[2,0]:=(m2[2,0]*m1[0,0])+(m2[2,1]*m1[1,0])+(m2[2,2]*m1[2,0]);
 result[2,1]:=(m2[2,0]*m1[0,1])+(m2[2,1]*m1[1,1])+(m2[2,2]*m1[2,1]);
 result[2,2]:=(m2[2,0]*m1[0,2])+(m2[2,1]*m1[1,2])+(m2[2,2]*m1[2,2]);
end;

procedure Matrix3x3ScalarMul(var m:TPhysicsMatrix3x3;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m[0,0]:=m[0,0]*s;
 m[0,1]:=m[0,1]*s;
 m[0,2]:=m[0,2]*s;
 m[1,0]:=m[1,0]*s;
 m[1,1]:=m[1,1]*s;
 m[1,2]:=m[1,2]*s;
 m[2,0]:=m[2,0]*s;
 m[2,1]:=m[2,1]*s;
 m[2,2]:=m[2,2]*s;
end;

function Matrix3x3TermScalarMul(const m:TPhysicsMatrix3x3;s:TPhysicsFloat):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=m[0,0]*s;
 result[0,1]:=m[0,1]*s;
 result[0,2]:=m[0,2]*s;
 result[1,0]:=m[1,0]*s;
 result[1,1]:=m[1,1]*s;
 result[1,2]:=m[1,2]*s;
 result[2,0]:=m[2,0]*s;
 result[2,1]:=m[2,1]*s;
 result[2,2]:=m[2,2]*s;
end;

procedure Matrix3x3Transpose(var m:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var mt:TPhysicsMatrix3x3;
begin
 mt[0,0]:=m[0,0];
 mt[1,0]:=m[0,1];
 mt[2,0]:=m[0,2];
 mt[0,1]:=m[1,0];
 mt[1,1]:=m[1,1];
 mt[2,1]:=m[1,2];
 mt[0,2]:=m[2,0];
 mt[1,2]:=m[2,1];
 mt[2,2]:=m[2,2];
 m:=mt;
end;

function Matrix3x3TermTranspose(const m:TPhysicsMatrix3x3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=m[0,0];
 result[1,0]:=m[0,1];
 result[2,0]:=m[0,2];
 result[0,1]:=m[1,0];
 result[1,1]:=m[1,1];
 result[2,1]:=m[1,2];
 result[0,2]:=m[2,0];
 result[1,2]:=m[2,1];
 result[2,2]:=m[2,2];
end;

function Matrix3x3Determinant(const m:TPhysicsMatrix3x3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var mat:TPhysicsMatrix3x3Ex absolute m;
begin
 result:=((((((mat[0]*mat[4]*mat[8])+(mat[3]*mat[7]*mat[2]))+(mat[6]*mat[1]*mat[5]))-(mat[6]*mat[4]*mat[2]))-(mat[3]*mat[1]*mat[8]))-(mat[0]*mat[7]*mat[5]));
end;

function Matrix3x3Euler(const m:TPhysicsMatrix3x3):TPhysicsEuler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var xangle,yangle,zangle,c,d,tx,ty:TPhysicsFloat;
begin
 d:=ArcSin(m[2,0]);
 yangle:=d;
 c:=cos(yangle);
 if abs(c)>0.005 then begin
  tx:=m[2,2]/c;
  ty:=-m[2,1]/c;
  xangle:=ArcTan2(ty,tx);
  tx:=m[0,0]/c;
  ty:=-m[1,0]/c;
  zangle:=ArcTan2(ty,tx);
 end else begin
  xangle:=0;
  tx:=m[1,1];
  ty:=m[0,1];
  zangle:=ArcTan2(ty,tx);
 end;
 if xangle<0 then xangle:=xangle+2*pi;
 if yangle<0 then yangle:=yangle+2*pi;
 if zangle<0 then zangle:=zangle+2*pi;
 result.Pitch:=xangle;
 result.Yaw:=yangle;
 result.Roll:=zangle;
end;

procedure Matrix3x3SetColumn(var m:TPhysicsMatrix3x3;const c:integer;const v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m[c,0]:=v.x;
 m[c,1]:=v.y;
 m[c,2]:=v.z;
end;

function Matrix3x3GetColumn(const m:TPhysicsMatrix3x3;const c:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=m[c,0];
 result.y:=m[c,1];
 result.z:=m[c,2];
end;

procedure Matrix3x3SetRow(var m:TPhysicsMatrix3x3;const r:integer;const v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m[0,r]:=v.x;
 m[1,r]:=v.y;
 m[2,r]:=v.z;
end;

function Matrix3x3GetRow(const m:TPhysicsMatrix3x3;const r:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=m[0,r];
 result.y:=m[1,r];
 result.z:=m[2,r];
end;

function Matrix3x3Compare(const m1,m2:TPhysicsMatrix3x3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var r,c:integer;
begin
 result:=true;
 for r:=0 to 2 do begin
  for c:=0 to 2 do begin
   if abs(m1[r,c]-m2[r,c])>EPSILON then begin
    result:=false;
    exit;
   end;
  end;
 end;
end;

function Matrix3x3Inverse(var mr:TPhysicsMatrix3x3;const ma:TPhysicsMatrix3x3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Det,idet:TPhysicsFloat;
    ret:TPhysicsMatrix3x3Ex absolute mr;
    mat:TPhysicsMatrix3x3Ex absolute ma;
begin
 det:=((((((mat[0]*mat[4]*mat[8])+(mat[3]*mat[7]*mat[2]))+(mat[6]*mat[1]*mat[5]))-(mat[6]*mat[4]*mat[2]))-(mat[3]*mat[1]*mat[8]))-(mat[0]*mat[7]*mat[5]));
 if abs(Det)<EPSILON then begin
  mr:=Matrix3x3Identity;
  result:=false;
 end else begin
  iDet:=1/Det;
  ret[0]:= (mat[4]*mat[8]-mat[7]*mat[5])*idet;
  ret[1]:=-(mat[1]*mat[8]-mat[7]*mat[2])*idet;
  ret[2]:= (mat[1]*mat[5]-mat[4]*mat[2])*idet;
  ret[3]:=-(mat[3]*mat[8]-mat[6]*mat[5])*idet;
  ret[4]:= (mat[0]*mat[8]-mat[6]*mat[2])*idet;
  ret[5]:=-(mat[0]*mat[5]-mat[3]*mat[2])*idet;
  ret[6]:= (mat[3]*mat[7]-mat[6]*mat[4])*idet;
  ret[7]:=-(mat[0]*mat[7]-mat[6]*mat[1])*idet;
  ret[8]:= (mat[0]*mat[4]-mat[3]*mat[1])*idet;
  result:=true;
 end;
end;

function Matrix3x3Map(const a,b:TPhysicsVector3):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Axis:TPhysicsVector3;
    Angle,Dot:TPhysicsFloat;
    Mat:TPhysicsMatrix3x3;
    AQuaternion:TPhysicsQuaternion;
begin
 Dot:=Vector3Dot(a,b);
 if abs(Dot-1)<EPSILON then begin
  Mat:=Matrix3x3Identity;
 end else begin
  Axis:=Vector3Cross(a,b);
  if abs(Dot+1)<EPSILON then begin
   AQuaternion:=QuaternionFromAxisAngle(Vector3(1,0,0),pi);
   Mat:=QuaternionToMatrix(AQuaternion);
  end else begin
   Angle:=ArcCos(Dot);
   if Vector3Compare(Axis,Vector3Origin) then begin
    Axis:=Vector3Cross(a,Vector3Norm(Vector3(b.y,a.z,a.x)));
    Angle:=pi;
   end;
   Vector3Normalize(Axis);
   AQuaternion:=QuaternionFromAxisAngle(Axis,Angle);
   Mat:=QuaternionToMatrix(AQuaternion);
  end;
 end;
 result:=Mat;
end;

procedure Matrix3x3OrthoNormalize(var m:TPhysicsMatrix3x3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var x,y,z:TPhysicsVector3;
    mat:TPhysicsMatrix3x3Ex absolute m;
begin
 x:=Vector3Norm(Vector3(mat[0],mat[1],mat[2]));
 y:=Vector3(mat[3],mat[4],mat[5]);
 z:=Vector3Norm(Vector3Cross(x,y));
 y:=Vector3Norm(Vector3Cross(z,x));
 mat[0]:=x.x;mat[3]:=y.x;mat[6]:=z.x;
 mat[1]:=x.y;mat[4]:=y.y;mat[7]:=z.y;
 mat[2]:=x.z;mat[5]:=y.z;mat[8]:=z.z;
end;

function Matrix4x4Set(m:TPhysicsMatrix3x3):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=m[0,0];
 result[0,1]:=m[0,1];
 result[0,2]:=m[0,2];
 result[0,3]:=0;
 result[1,0]:=m[1,0];
 result[1,1]:=m[1,1];
 result[1,2]:=m[1,2];
 result[1,3]:=0;
 result[2,0]:=m[2,0];
 result[2,1]:=m[2,1];
 result[2,2]:=m[2,2];
 result[2,3]:=0;
 result[3,0]:=0;
 result[3,1]:=0;
 result[3,2]:=0;
 result[3,3]:=1;
end;

function Matrix4x4Rotation(m:TPhysicsMatrix4x4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=m[0,0];
 result[0,1]:=m[0,1];
 result[0,2]:=m[0,2];
 result[0,3]:=0;
 result[1,0]:=m[1,0];
 result[1,1]:=m[1,1];
 result[1,2]:=m[1,2];
 result[1,3]:=0;
 result[2,0]:=m[2,0];
 result[2,1]:=m[2,1];
 result[2,2]:=m[2,2];
 result[2,3]:=0;
 result[3,0]:=0;
 result[3,1]:=0;
 result[3,2]:=0;
 result[3,3]:=1;
end;

function Matrix4x4RotateX(Angle:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix4x4Identity;
 result[1,1]:=cos(Angle);
 result[2,2]:=result[1,1];
 result[1,2]:=sin(Angle);
 result[2,1]:=-result[1,2];
end;

function Matrix4x4RotateY(Angle:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix4x4Identity;
 result[0,0]:=cos(Angle);
 result[2,2]:=result[0,0];
 result[0,2]:=-sin(Angle);
 result[2,0]:=-result[0,2];
end;

function Matrix4x4RotateZ(Angle:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix4x4Identity;
 result[0,0]:=cos(Angle);
 result[1,1]:=result[0,0];
 result[0,1]:=sin(Angle);
 result[1,0]:=-result[0,1];
end;

function Matrix4x4Rotate(const Euler:TPhysicsEuler):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var a,b,c,d,e,f,ad,bd:TPhysicsFloat;
begin
 a:=cos(Euler.Pitch);
 b:=sin(Euler.Pitch);
 c:=cos(Euler.Yaw);
 d:=sin(Euler.Yaw);
 e:=cos(Euler.Roll);
 f:=sin(Euler.Roll);
 ad:=a*d;
 bd:=b*d;
 result:=Matrix4x4Identity;
 result[0,0]:=c*e;
 result[1,0]:=-c*f;
 result[2,0]:=d;
 result[0,1]:=bd*e+a*f;
 result[1,1]:=-bd*f+a*e;
 result[2,1]:=-b*c;
 result[0,2]:=-ad*e+b*f;
 result[1,2]:=ad*f+b*e;
 result[2,2]:=a*c;
end;

function Matrix4x4Rotate(Angle:TPhysicsFloat;Axis:TPhysicsVector4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var m:TPhysicsMatrix4x4;
    CosinusAngle,SinusAngle:TPhysicsFloat;
begin
 m:=Matrix4x4Identity;
 CosinusAngle:=cos(Angle);
 SinusAngle:=sin(Angle);
 m[0,0]:=CosinusAngle+(1-CosinusAngle)*Axis.x*Axis.x;
 m[1,0]:=(1-CosinusAngle)*Axis.x*Axis.y-Axis.z*SinusAngle;
 m[2,0]:=(1-CosinusAngle)*Axis.x*Axis.z+Axis.y*SinusAngle;
 m[0,1]:=(1-CosinusAngle)*Axis.x*Axis.z+Axis.z*SinusAngle;
 m[1,1]:=CosinusAngle+(1-CosinusAngle)*Axis.y*Axis.y;
 m[2,1]:=(1-CosinusAngle)*Axis.y*Axis.z-Axis.x*SinusAngle;
 m[0,2]:=(1-CosinusAngle)*Axis.x*Axis.z-Axis.y*SinusAngle;
 m[1,2]:=(1-CosinusAngle)*Axis.y*Axis.z+Axis.x*SinusAngle;
 m[2,2]:=CosinusAngle+(1-CosinusAngle)*Axis.z*Axis.z;
 result:=m;
end;

function Matrix4x4Translate(x,y,z:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=Matrix4x4Identity;
 result[3,0]:=x;
 result[3,1]:=y;
 result[3,2]:=z;
end;

function Matrix4x4Translate(const v:TPhysicsVector3):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=Matrix4x4Identity;
 result[3,0]:=v.x;
 result[3,1]:=v.y;
 result[3,2]:=v.z;
end;

function Matrix4x4Translate(const v:TPhysicsVector4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=Matrix4x4Identity;
 result[3,0]:=v.x;
 result[3,1]:=v.y;
 result[3,2]:=v.z;
end;

procedure Matrix4x4Translate(var m:TPhysicsMatrix4x4;const v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 m[3,0]:=m[0,0]*v.x+m[1,0]*v.y+m[2,0]*v.z+m[3,0];
 m[3,1]:=m[0,1]*v.x+m[1,1]*v.y+m[2,1]*v.z+m[3,1];
 m[3,2]:=m[0,2]*v.x+m[1,2]*v.y+m[2,2]*v.z+m[3,2];
 m[3,3]:=m[0,3]*v.x+m[1,3]*v.y+m[2,3]*v.z+m[3,3];
end;

procedure Matrix4x4Translate(var m:TPhysicsMatrix4x4;const v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 m[3,0]:=m[0,0]*v.x+m[1,0]*v.y+m[2,0]*v.z+m[3,0]*v.w;
 m[3,1]:=m[0,1]*v.x+m[1,1]*v.y+m[2,1]*v.z+m[3,1]*v.w;
 m[3,2]:=m[0,2]*v.x+m[1,2]*v.y+m[2,2]*v.z+m[3,2]*v.w;
 m[3,3]:=m[0,3]*v.x+m[1,3]*v.y+m[2,3]*v.z+m[3,3]*v.w;
end;

function Matrix4x4Scale(sx,sy,sz:TPhysicsFloat):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Matrix4x4Identity;
 result[0,0]:=sx;
 result[1,1]:=sy;
 result[2,2]:=sz;
end;

procedure Matrix4x4Add(var m1:TPhysicsMatrix4x4;const m2:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m1[0,0]:=m1[0,0]+m2[0,0];
 m1[0,1]:=m1[0,1]+m2[0,1];
 m1[0,2]:=m1[0,2]+m2[0,2];
 m1[0,3]:=m1[0,3]+m2[0,3];
 m1[1,0]:=m1[1,0]+m2[1,0];
 m1[1,1]:=m1[1,1]+m2[1,1];
 m1[1,2]:=m1[1,2]+m2[1,2];
 m1[1,3]:=m1[1,3]+m2[1,3];
 m1[2,0]:=m1[2,0]+m2[2,0];
 m1[2,1]:=m1[2,1]+m2[2,1];
 m1[2,2]:=m1[2,2]+m2[2,2];
 m1[2,3]:=m1[2,3]+m2[2,3];
 m1[3,0]:=m1[3,0]+m2[3,0];
 m1[3,1]:=m1[3,1]+m2[3,1];
 m1[3,2]:=m1[3,2]+m2[3,2];
 m1[3,3]:=m1[3,3]+m2[3,3];
end;

procedure Matrix4x4Sub(var m1:TPhysicsMatrix4x4;const m2:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m1[0,0]:=m1[0,0]-m2[0,0];
 m1[0,1]:=m1[0,1]-m2[0,1];
 m1[0,2]:=m1[0,2]-m2[0,2];
 m1[0,3]:=m1[0,3]-m2[0,3];
 m1[1,0]:=m1[1,0]-m2[1,0];
 m1[1,1]:=m1[1,1]-m2[1,1];
 m1[1,2]:=m1[1,2]-m2[1,2];
 m1[1,3]:=m1[1,3]-m2[1,3];
 m1[2,0]:=m1[2,0]-m2[2,0];
 m1[2,1]:=m1[2,1]-m2[2,1];
 m1[2,2]:=m1[2,2]-m2[2,2];
 m1[2,3]:=m1[2,3]-m2[2,3];
 m1[3,0]:=m1[3,0]-m2[3,0];
 m1[3,1]:=m1[3,1]-m2[3,1];
 m1[3,2]:=m1[3,2]-m2[3,2];
 m1[3,3]:=m1[3,3]-m2[3,3];
end;

procedure Matrix4x4Mul(var m1:TPhysicsMatrix4x4;const m2:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var t:TPhysicsMatrix4x4;
begin
 t[0,0]:=(m2[0,0]*m1[0,0])+(m2[0,1]*m1[1,0])+(m2[0,2]*m1[2,0])+(m2[0,3]*m1[3,0]);
 t[0,1]:=(m2[0,0]*m1[0,1])+(m2[0,1]*m1[1,1])+(m2[0,2]*m1[2,1])+(m2[0,3]*m1[3,1]);
 t[0,2]:=(m2[0,0]*m1[0,2])+(m2[0,1]*m1[1,2])+(m2[0,2]*m1[2,2])+(m2[0,3]*m1[3,2]);
 t[0,3]:=(m2[0,0]*m1[0,3])+(m2[0,1]*m1[1,3])+(m2[0,2]*m1[2,3])+(m2[0,3]*m1[3,3]);
 t[1,0]:=(m2[1,0]*m1[0,0])+(m2[1,1]*m1[1,0])+(m2[1,2]*m1[2,0])+(m2[1,3]*m1[3,0]);
 t[1,1]:=(m2[1,0]*m1[0,1])+(m2[1,1]*m1[1,1])+(m2[1,2]*m1[2,1])+(m2[1,3]*m1[3,1]);
 t[1,2]:=(m2[1,0]*m1[0,2])+(m2[1,1]*m1[1,2])+(m2[1,2]*m1[2,2])+(m2[1,3]*m1[3,2]);
 t[1,3]:=(m2[1,0]*m1[0,3])+(m2[1,1]*m1[1,3])+(m2[1,2]*m1[2,3])+(m2[1,3]*m1[3,3]);
 t[2,0]:=(m2[2,0]*m1[0,0])+(m2[2,1]*m1[1,0])+(m2[2,2]*m1[2,0])+(m2[2,3]*m1[3,0]);
 t[2,1]:=(m2[2,0]*m1[0,1])+(m2[2,1]*m1[1,1])+(m2[2,2]*m1[2,1])+(m2[2,3]*m1[3,1]);
 t[2,2]:=(m2[2,0]*m1[0,2])+(m2[2,1]*m1[1,2])+(m2[2,2]*m1[2,2])+(m2[2,3]*m1[3,2]);
 t[2,3]:=(m2[2,0]*m1[0,3])+(m2[2,1]*m1[1,3])+(m2[2,2]*m1[2,3])+(m2[2,3]*m1[3,3]);
 t[3,0]:=(m2[3,0]*m1[0,0])+(m2[3,1]*m1[1,0])+(m2[3,2]*m1[2,0])+(m2[3,3]*m1[3,0]);
 t[3,1]:=(m2[3,0]*m1[0,1])+(m2[3,1]*m1[1,1])+(m2[3,2]*m1[2,1])+(m2[3,3]*m1[3,1]);
 t[3,2]:=(m2[3,0]*m1[0,2])+(m2[3,1]*m1[1,2])+(m2[3,2]*m1[2,2])+(m2[3,3]*m1[3,2]);
 t[3,3]:=(m2[3,0]*m1[0,3])+(m2[3,1]*m1[1,3])+(m2[3,2]*m1[2,3])+(m2[3,3]*m1[3,3]);
 m1:=t;
end;

function Matrix4x4TermMul(const m1,m2:TPhysicsMatrix4x4):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=(m2[0,0]*m1[0,0])+(m2[0,1]*m1[1,0])+(m2[0,2]*m1[2,0])+(m2[0,3]*m1[3,0]);
 result[0,1]:=(m2[0,0]*m1[0,1])+(m2[0,1]*m1[1,1])+(m2[0,2]*m1[2,1])+(m2[0,3]*m1[3,1]);
 result[0,2]:=(m2[0,0]*m1[0,2])+(m2[0,1]*m1[1,2])+(m2[0,2]*m1[2,2])+(m2[0,3]*m1[3,2]);
 result[0,3]:=(m2[0,0]*m1[0,3])+(m2[0,1]*m1[1,3])+(m2[0,2]*m1[2,3])+(m2[0,3]*m1[3,3]);
 result[1,0]:=(m2[1,0]*m1[0,0])+(m2[1,1]*m1[1,0])+(m2[1,2]*m1[2,0])+(m2[1,3]*m1[3,0]);
 result[1,1]:=(m2[1,0]*m1[0,1])+(m2[1,1]*m1[1,1])+(m2[1,2]*m1[2,1])+(m2[1,3]*m1[3,1]);
 result[1,2]:=(m2[1,0]*m1[0,2])+(m2[1,1]*m1[1,2])+(m2[1,2]*m1[2,2])+(m2[1,3]*m1[3,2]);
 result[1,3]:=(m2[1,0]*m1[0,3])+(m2[1,1]*m1[1,3])+(m2[1,2]*m1[2,3])+(m2[1,3]*m1[3,3]);
 result[2,0]:=(m2[2,0]*m1[0,0])+(m2[2,1]*m1[1,0])+(m2[2,2]*m1[2,0])+(m2[2,3]*m1[3,0]);
 result[2,1]:=(m2[2,0]*m1[0,1])+(m2[2,1]*m1[1,1])+(m2[2,2]*m1[2,1])+(m2[2,3]*m1[3,1]);
 result[2,2]:=(m2[2,0]*m1[0,2])+(m2[2,1]*m1[1,2])+(m2[2,2]*m1[2,2])+(m2[2,3]*m1[3,2]);
 result[2,3]:=(m2[2,0]*m1[0,3])+(m2[2,1]*m1[1,3])+(m2[2,2]*m1[2,3])+(m2[2,3]*m1[3,3]);
 result[3,0]:=(m2[3,0]*m1[0,0])+(m2[3,1]*m1[1,0])+(m2[3,2]*m1[2,0])+(m2[3,3]*m1[3,0]);
 result[3,1]:=(m2[3,0]*m1[0,1])+(m2[3,1]*m1[1,1])+(m2[3,2]*m1[2,1])+(m2[3,3]*m1[3,1]);
 result[3,2]:=(m2[3,0]*m1[0,2])+(m2[3,1]*m1[1,2])+(m2[3,2]*m1[2,2])+(m2[3,3]*m1[3,2]);
 result[3,3]:=(m2[3,0]*m1[0,3])+(m2[3,1]*m1[1,3])+(m2[3,2]*m1[2,3])+(m2[3,3]*m1[3,3]);
end;

procedure Matrix4x4ScalarMul(var m:TPhysicsMatrix4x4;s:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m[0,0]:=m[0,0]*s;
 m[0,1]:=m[0,1]*s;
 m[0,2]:=m[0,2]*s;
 m[0,3]:=m[0,3]*s;
 m[1,0]:=m[1,0]*s;
 m[1,1]:=m[1,1]*s;
 m[1,2]:=m[1,2]*s;
 m[1,3]:=m[1,3]*s;
 m[2,0]:=m[2,0]*s;
 m[2,1]:=m[2,1]*s;
 m[2,2]:=m[2,2]*s;
 m[2,3]:=m[2,3]*s;
 m[3,0]:=m[3,0]*s;
 m[3,1]:=m[3,1]*s;
 m[3,2]:=m[3,2]*s;
 m[3,3]:=m[3,3]*s;
end;

procedure Matrix4x4Transpose(var m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var mt:TPhysicsMatrix4x4;
begin
 mt[0,0]:=m[0,0];
 mt[0,1]:=m[1,0];
 mt[0,2]:=m[2,0];
 mt[0,3]:=m[3,0];
 mt[1,0]:=m[0,1];
 mt[1,1]:=m[1,1];
 mt[1,2]:=m[2,1];
 mt[1,3]:=m[3,1];
 mt[2,0]:=m[0,2];
 mt[2,1]:=m[1,2];
 mt[2,2]:=m[2,2];
 mt[2,3]:=m[3,2];
 mt[3,0]:=m[0,3];
 mt[3,1]:=m[1,3];
 mt[3,2]:=m[2,3];
 mt[3,3]:=m[3,3];
 m:=mt;
end;

function Matrix4x4Determinant(const m:TPhysicsMatrix4x4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var mat:TPhysicsMatrix4x4Ex absolute m;
begin
 result:=((((((((mat[0]*mat[5]*mat[10])+(mat[4]*mat[9]*mat[2])))+(mat[8]*mat[1]*mat[6]))-(mat[8]*mat[5]*mat[2])))-(mat[4]*mat[1]*mat[10]))-(mat[0]*mat[9]*mat[6]));
end;

function Matrix4x4Euler(const m:TPhysicsMatrix4x4):TPhysicsEuler; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var xangle,yangle,zangle,c,d,tx,ty:TPhysicsFloat;
begin
 d:=ArcSin(m[2,0]);
 yangle:=d;
 c:=cos(yangle);
 if abs(c)>0.005 then begin
  tx:=m[2,2]/c;
  ty:=-m[2,1]/c;
  xangle:=ArcTan2(ty,tx);
  tx:=m[0,0]/c;
  ty:=-m[1,0]/c;
  zangle:=ArcTan2(ty,tx);
 end else begin
  xangle:=0;
  tx:=m[1,1];
  ty:=m[0,1];
  zangle:=ArcTan2(ty,tx);
 end;
 if xangle<0 then xangle:=xangle+2*pi;
 if yangle<0 then yangle:=yangle+2*pi;
 if zangle<0 then zangle:=zangle+2*pi;
 result.Pitch:=xangle;
 result.Yaw:=yangle;
 result.Roll:=zangle;
end;

procedure Matrix4x4SetColumn(var m:TPhysicsMatrix4x4;const c:integer;const v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m[c,0]:=v.x;
 m[c,1]:=v.y;
 m[c,2]:=v.z;
 m[c,3]:=v.w;
end;

function Matrix4x4GetColumn(const m:TPhysicsMatrix4x4;const c:integer):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=m[c,0];
 result.y:=m[c,1];
 result.z:=m[c,2];
 result.w:=m[c,3];
end;

procedure Matrix4x4SetRow(var m:TPhysicsMatrix4x4;const r:integer;const v:TPhysicsVector4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 m[0,r]:=v.x;
 m[1,r]:=v.y;
 m[2,r]:=v.z;
 m[3,r]:=v.w;
end;

function Matrix4x4GetRow(const m:TPhysicsMatrix4x4;const r:integer):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=m[0,r];
 result.y:=m[1,r];
 result.z:=m[2,r];
 result.w:=m[3,r];
end;

function Matrix4x4Compare(const m1,m2:TPhysicsMatrix4x4):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var r,c:integer;
begin
 result:=true;
 for r:=0 to 3 do begin
  for c:=0 to 3 do begin
   if abs(m1[r,c]-m2[r,c])>EPSILON then begin
    result:=false;
    exit;
   end;
  end;
 end;
end;

function Matrix4x4Inverse(var mr:TPhysicsMatrix4x4;const ma:TPhysicsMatrix4x4):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Det,IDet:TPhysicsFloat;
    ret:TPhysicsMatrix4x4Ex absolute mr;
    mat:TPhysicsMatrix4x4Ex absolute ma;
begin
 det:=((((((((mat[0]*mat[5]*mat[10])+(mat[4]*mat[9]*mat[2])))+(mat[8]*mat[1]*mat[6]))-(mat[8]*mat[5]*mat[2])))-(mat[4]*mat[1]*mat[10]))-(mat[0]*mat[9]*mat[6]));
 if abs(Det)<EPSILON then begin
  mr:=Matrix4x4Identity;
  result:=false;
 end else begin
  IDet:=1/Det;
  ret[0]:= (mat[5]*mat[10]-mat[9]*mat[6])*idet;
  ret[1]:=-(mat[1]*mat[10]-mat[9]*mat[2])*idet;
  ret[2]:= (mat[1]*mat[6]-mat[5]*mat[2])*idet;
  ret[3]:=0.0;
  ret[4]:=-(mat[4]*mat[10]-mat[8]*mat[6])*idet;
  ret[5]:= (mat[0]*mat[10]-mat[8]*mat[2])*idet;
  ret[6]:=-(mat[0]*mat[6]-mat[4]*mat[2])*idet;
  ret[7]:=0.0;
  ret[8]:= (mat[4]*mat[9]-mat[8]*mat[5])*idet;
  ret[9]:=-(mat[0]*mat[9]-mat[8]*mat[1])*idet;
  ret[10]:= (mat[0]*mat[5]-mat[4]*mat[1])*idet;
  ret[11]:=0.0;
  ret[12]:=-(mat[12]*ret[0]+mat[13]*ret[4]+mat[14]*ret[8]);
  ret[13]:=-(mat[12]*ret[1]+mat[13]*ret[5]+mat[14]*ret[9]);
  ret[14]:=-(mat[12]*ret[2]+mat[13]*ret[6]+mat[14]*ret[10]);
  ret[15]:=1.0;
  result:=true;
 end;
end;

function Matrix4x4GetSubMatrix3x3Ex(const m:TPhysicsMatrix4x4;i,j:integer):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var DI,DJ,SI,SJ:integer;
begin
 for DI:=0 to 2 do begin
  for DJ:=0 to 2 do begin
   if DI>=i then begin
    SI:=DI+1;
   end else begin
    SI:=DI;
   end;
   if DJ>=j then begin
    SJ:=DJ+1;
   end else begin
    SJ:=DJ;
   end;
   result[DI,DJ]:=m[SI,SJ];
  end;
 end;
end;
                             
function Matrix4x4GetSubMatrix3x3(const m:TPhysicsMatrix4x4;i,j:integer):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var DI,DJ,SI,SJ:integer;
begin
 for DI:=0 to 2 do begin
  for DJ:=0 to 2 do begin
   SI:=DI+i;
   while SI>3 do begin
    dec(SI,4);
   end;
   SJ:=DJ+j;
   while SJ>3 do begin
    dec(SJ,4);
   end;
   result[DI,DJ]:=m[SI,SJ];
  end;
 end;
end;

function Matrix4x4Perspective(fovy,Aspect,zNear,zFar:double):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Sine,Cotangent,ZDelta,Radians:double;
begin
 Radians:=(fovy/2)*DEG2RAD;
 ZDelta:=zFar-zNear;
 Sine:=sin(Radians);
 if (ZDelta=0) or (Sine=0) or (aspect=0) then exit;
 Cotangent:=cos(Radians)/Sine;
 result:=Matrix4x4Identity;
 result[0][0]:=Cotangent/aspect;
 result[1][1]:=Cotangent;
 result[2][2]:=-(zFar+zNear)/ZDelta;
 result[2][3]:=-1;
 result[3][2]:=-2*zNear*zFar/ZDelta;
 result[3][3]:=0;
end;

function Matrix4x4LookAt(const Eye,Center,Up:TPhysicsVector3):TPhysicsMatrix4x4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var x,y,z:TPhysicsVector3;
begin
 z:=Vector3Norm(Vector3Sub(Eye,Center));
 y:=Up;
 x:=Vector3Norm(Vector3Cross(y,z));
 y:=Vector3Norm(Vector3Cross(z,x));
 result:=Matrix4x4Identity;
 result[0,0]:=x.x;
 result[1,0]:=x.y;
 result[2,0]:=x.z;
 result[0,1]:=y.x;
 result[1,1]:=y.y;
 result[2,1]:=y.z;
 result[0,2]:=z.x;
 result[1,2]:=z.y;
 result[2,2]:=z.z;
 Matrix4x4Translate(result,Vector3Neg(Eye));
end;

function PlaneVectorDistance(const Plane:TPhysicsPlane;const Point:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=(Plane.a*Point.x)+(Plane.b*Point.y)+(Plane.c*Point.z)+Plane.d;
end;

function PlaneVectorDistance(const Plane:TPhysicsPlane;const Point:TPhysicsVector4):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=(Plane.a*Point.x)+(Plane.b*Point.y)+(Plane.c*Point.z)+(Plane.d*Point.w);
end;

function PlaneFromEdgePoints(P1,P2,N:TPhysicsVector3):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var d:TPhysicsVector3;
begin
 d:=Vector3Sub(P2,P1);
 result.Normal:=Vector3Norm(Vector3Cross(d,n));
 result.Distance:=-Vector3Dot(P1,result.Normal);
end;

function PlaneFromEdgePoints(P1,P2,N:TPhysicsVector4):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var d,tn:TPhysicsVector4;
begin
 d:=Vector4Sub(P2,P1);
 tn:=Vector4Norm(Vector4Cross(d,n));
 result.Normal:=Vector3(tn.x,tn.y,tn.z);
 result.Distance:=-Vector3Dot(Vector3(P1.x,P1.y,P1.z),result.Normal);
end;

function PlaneFromPoints(P1,P2,P3:TPhysicsVector3):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var n:TPhysicsVector3;
begin                   
 n:=Vector3Norm(Vector3Cross(Vector3Sub(P2,P1),Vector3Sub(P3,P1)));
 result.a:=n.x;
 result.b:=n.y;
 result.c:=n.z;
 result.d:=-((result.a*P1.x)+(result.b*P1.y)+(result.c*P1.z));
end;

function PlaneFromPoints(P1,P2,P3:TPhysicsVector4):TPhysicsPlane; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var n:TPhysicsVector4;
begin
 n:=Vector4Norm(Vector4Cross(Vector4Sub(P2,P1),Vector4Sub(P3,P1)));
 result.a:=n.x;
 result.b:=n.y;
 result.c:=n.z;
 result.d:=-((result.a*P1.x)+(result.b*P1.y)+(result.c*P1.z));
end;

function QuaternionLengthSquared(const AQuaternion:TPhysicsQuaternion):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sqr(AQuaternion.w)+sqr(AQuaternion.x)+sqr(AQuaternion.y)+sqr(AQuaternion.z);
end;

function QuaternionNormal(const AQuaternion:TPhysicsQuaternion):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=sqrt(sqr(AQuaternion.w)+sqr(AQuaternion.x)+sqr(AQuaternion.y)+sqr(AQuaternion.z));
end;

procedure QuaternionNormalize(var AQuaternion:TPhysicsQuaternion); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Normal:TPhysicsFloat;
begin
 Normal:=QuaternionNormal(AQuaternion);
 if abs(Normal)>EPSILON then begin
  Normal:=1/Normal;
  AQuaternion.x:=AQuaternion.x*Normal;
  AQuaternion.y:=AQuaternion.y*Normal;
  AQuaternion.z:=AQuaternion.z*Normal;
  AQuaternion.w:=AQuaternion.w*Normal;
 end;
end;

function QuaternionMul(const q1,q2:TPhysicsQuaternion):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var v1,v2,QuaternionVector:TPhysicsVector3;
    qr:TPhysicsQuaternion;
begin
 v1:=Vector3(q1.x,q1.y,q1.z);
 v2:=Vector3(q2.x,q2.y,q2.z);
 qr.w:=q1.w*q2.w-Vector3Dot(v1,v2);
 QuaternionVector:=Vector3Add(Vector3ScalarMul(v2,q1.w),Vector3Add(Vector3ScalarMul(v1,q2.w),Vector3Cross(v1,v2)));
 qr.x:=QuaternionVector.x;
 qr.y:=QuaternionVector.y;
 qr.z:=QuaternionVector.z;
 QuaternionNormalize(qr);
 result:=qr;
end;

function QuaternionFromAxisAngle(const Axis:TPhysicsVector3;Angle:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var sa2:TPhysicsFloat;
begin
 result.w:=cos(Angle/2);
 sa2:=sin(Angle/2);
 result.x:=Axis.x*sa2;
 result.y:=Axis.y*sa2;
 result.z:=Axis.z*sa2;
 QuaternionNormalize(result);
end;

function QuaternionFromEuler(pitch,yaw,roll:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var q1,q2,q3:TPhysicsQuaternion;
begin
 q1:=Quaternion(cos(pitch/2),sin(pitch/2),0,0);
 q2:=Quaternion(cos(yaw/2),0,sin(yaw/2),0);
 q3:=Quaternion(cos(roll/2),0,0,sin(roll/2));
 result:=QuaternionMul(QuaternionMul(q3,q2),q1);
end;

function QuaternionToMatrix(const AQuaternion:TPhysicsQuaternion):TPhysicsMatrix3x3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0,0]:=1-2*AQuaternion.y*AQuaternion.y-2*AQuaternion.z*AQuaternion.z;
 result[0,1]:=2*AQuaternion.x*AQuaternion.y+2*AQuaternion.w*AQuaternion.z;
 result[0,2]:=2*AQuaternion.x*AQuaternion.z-2*AQuaternion.w*AQuaternion.y;
 result[1,0]:=2*AQuaternion.x*AQuaternion.y-2*AQuaternion.w*AQuaternion.z;
 result[1,1]:=1-2*AQuaternion.x*AQuaternion.x-2*AQuaternion.z*AQuaternion.z;
 result[1,2]:=2*AQuaternion.y*AQuaternion.z+2*AQuaternion.w*AQuaternion.x;
 result[2,0]:=2*AQuaternion.x*AQuaternion.z+2*AQuaternion.w*AQuaternion.y;
 result[2,1]:=2*AQuaternion.y*AQuaternion.z-2*AQuaternion.w*AQuaternion.x;
 result[2,2]:=1-2*AQuaternion.x*AQuaternion.x-2*AQuaternion.y*AQuaternion.y;
end;

procedure QuaternionToAxisAngle(const AQuaternion:TPhysicsQuaternion;var Axis:TPhysicsVector3;var Angle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var s:TPhysicsFloat;
begin
 s:=sqr(AQuaternion.x)+sqr(AQuaternion.y)+sqr(AQuaternion.z);
 Angle:=2*ArcCos(AQuaternion.w);
 Axis.x:=AQuaternion.x/s;
 Axis.y:=AQuaternion.y/s;
 Axis.z:=AQuaternion.z/s;
end;

function QuaternionSlerp(q1,q2:TPhysicsQuaternion;t:TPhysicsFloat):TPhysicsQuaternion; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var AQuaternion:array[0..3] of TPhysicsFloat;
    Omega,co,so,s0,s1:TPhysicsFloat;
begin
 co:=q1.x*q2.x+q1.y*q2.y+q1.z*q2.z+q1.w*q2.w;
 if co<0 then begin
  co:=-co;
  AQuaternion[0]:=-q2.x;
  AQuaternion[1]:=-q2.y;
  AQuaternion[2]:=-q2.z;
  AQuaternion[3]:=-q2.w;
 end else begin
  AQuaternion[0]:=q2.x;
  AQuaternion[1]:=q2.y;
  AQuaternion[2]:=q2.z;
  AQuaternion[3]:=q2.w;
 end;
 if (1-co)>EPSILON then begin
  Omega:=ArcCos(co);
  so:=sin(Omega);
  s0:=sin((1.0-t)*Omega)/so;
  s1:=sin(t*Omega)/so;
 end else begin
  s0:=1-t;
  s1:=t;
 end;
 result.x:=(s0*q1.x)+(s1*AQuaternion[0]);
 result.y:=(s0*q1.y)+(s1*AQuaternion[1]);
 result.z:=(s0*q1.z)+(s1*AQuaternion[2]);
 result.w:=(s0*q1.w)+(s1*AQuaternion[3]);
end;

function SphereCoordsFromCartesianVector3(const v:TPhysicsVector3):TPhysicsSphereCoords; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.Radius:=Vector3Length(v);
 result.Theta:=ArcCos(v.z/result.Radius);
 result.Phi:=GetSign(v.y)*ArcCos(v.x/sqrt(v.x*v.x+v.y*v.y));
end;

function SphereCoordsFromCartesianVector4(const v:TPhysicsVector4):TPhysicsSphereCoords; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.Radius:=Vector4Length(v);
 result.Theta:=ArcCos(v.z/result.Radius);
 result.Phi:=GetSign(v.y)*ArcCos(v.x/sqrt(v.x*v.x+v.y*v.y));
end;

function SphereCoordsToCartesianVector3(const s:TPhysicsSphereCoords):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=s.Radius*sin(s.Theta)*cos(s.Phi);
 result.y:=s.Radius*sin(s.Theta)*sin(s.Phi);
 result.z:=s.Radius*cos(s.Theta);
end;

function SphereCoordsToCartesianVector4(const s:TPhysicsSphereCoords):TPhysicsVector4; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.x:=s.Radius*sin(s.Theta)*cos(s.Phi);
 result.y:=s.Radius*sin(s.Theta)*sin(s.Phi);
 result.z:=s.Radius*cos(s.Theta);
 result.w:=1;
end;

function CullSphere(const s:TPhysicsSphere;const p:array of TPhysicsPlane):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
begin
 result:=true;
 for i:=0 to length(p)-1 do begin
  if PlaneVectorDistance(p[i],s.Center)<-s.Radius then begin
   result:=false;
   exit;
  end;
 end;
end;

function SphereContains(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=(a.Radius>=b.Radius) and (Vector3Length(Vector3Sub(a.Center,b.Center))<=(a.Radius-b.Radius));
end;

function SphereContainsEx(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=((a.Radius+EPSILON)>=(b.Radius-EPSILON)) and (Vector3Length(Vector3Sub(a.Center,b.Center))<=((a.Radius+EPSILON)-(b.Radius-EPSILON)));
end;

function SphereIntersect(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Vector3Length(Vector3Sub(a.Center,b.Center))<=(a.Radius+b.Radius);
end;

function SphereIntersectEx(const a,b:TPhysicsSphere):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Vector3Length(Vector3Sub(a.Center,b.Center))<=(a.Radius+b.Radius+(EPSILON*2));
end;

function SphereRayIntersect(var s:TPhysicsSphere;const Origin,Direction:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var m:TPhysicsVector3;
    p,d:TPhysicsFloat;
begin
 result:=false;
 m:=Vector3Sub(Origin,s.Center);
 p:=-Vector3Dot(m,Direction);
 d:=sqr(p)-Vector3LengthSquared(m)+sqr(s.Radius);
 if d<=0 then exit;
 result:=(p+sqrt(d))>0;
end;

function SphereFromAABB(const AABB:TPhysicsAABB):TPhysicsSphere; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.Center:=Vector3ScalarMul(Vector3Add(AABB.Min,AABB.Max),0.5);
 result.Radius:=Max(Vector3Length(Vector3Sub(AABB.Max,result.Center)),Vector3Length(Vector3Sub(AABB.Min,result.Center)));
end;

function SphereExtend(const Sphere,WithSphere:TPhysicsSphere):TPhysicsSphere; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var x0,y0,z0,r0,x1,y1,z1,r1,xn,yn,zn,dn,t:TPhysicsFloat;
begin
 x0:=Sphere.Center.x;
 y0:=Sphere.Center.y;
 z0:=Sphere.Center.z;
 r0:=Sphere.Radius;

 x1:=WithSphere.Center.x;
 y1:=WithSphere.Center.y;
 z1:=WithSphere.Center.z;
 r1:=WithSphere.Radius;

 xn:=x1-x0;
 yn:=y1-y0;
 zn:=z1-z0;
 dn:=sqrt(sqr(xn)+sqr(yn)+sqr(zn));
 if abs(dn)<EPSILON then begin
  result:=Sphere;
  exit;
 end;

 if (dn+r1)<r0 then begin
  result:=Sphere;
  exit;
 end;

 result.Radius:=(dn+r0+r1)*0.5;
 t:=(result.Radius-r0)/dn;
 result.Center.x:=x0+(xn*t);
 result.Center.y:=y0+(xn*t);
 result.Center.z:=z0+(xn*t);
end;
     
function AABBResize(const AABB:TPhysicsAABB;f:TPhysicsFloat):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var v:TPhysicsVector3;
begin
 v:=Vector3ScalarMul(Vector3Sub(AABB.Max,AABB.Min),f);
 result.Min:=Vector3Sub(AABB.Min,v);
 result.Max:=Vector3Add(AABB.Max,v);
end;

function AABBCombine(const AABB,WithAABB:TPhysicsAABB):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result.Min.x:=Min(AABB.Min.x,WithAABB.Min.x);
 result.Min.y:=Min(AABB.Min.y,WithAABB.Min.y);
 result.Min.z:=Min(AABB.Min.z,WithAABB.Min.z);
 result.Max.x:=Max(AABB.Max.x,WithAABB.Max.x);
 result.Max.y:=Max(AABB.Max.y,WithAABB.Max.y);
 result.Max.z:=Max(AABB.Max.z,WithAABB.Max.z);
end;

function AABBIntersect(const AABB,WithAABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=((AABB.Max.x>=WithAABB.Min.x) and (AABB.Min.x<=WithAABB.Max.x)) and
         ((AABB.Max.y>=WithAABB.Min.y) and (AABB.Min.y<=WithAABB.Max.y)) and
         ((AABB.Max.z>=WithAABB.Min.z) and (AABB.Min.z<=WithAABB.Max.z));
end;

function AABBIntersectEx(const AABB,WithAABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=(((AABB.Max.x+AABBEPSILON)>=(WithAABB.Min.x-AABBEPSILON)) and ((AABB.Min.x-AABBEPSILON)<=(WithAABB.Max.x+AABBEPSILON))) and
         (((AABB.Max.y+AABBEPSILON)>=(WithAABB.Min.y-AABBEPSILON)) and ((AABB.Min.y-AABBEPSILON)<=(WithAABB.Max.y+AABBEPSILON))) and
         (((AABB.Max.z+AABBEPSILON)>=(WithAABB.Min.z-AABBEPSILON)) and ((AABB.Min.z-AABBEPSILON)<=(WithAABB.Max.z+AABBEPSILON)));
end;

function AABBContains(const InAABB,AABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=(InAABB.Min.x<=AABB.Min.x) and (InAABB.Min.y<=AABB.Min.y) and (InAABB.Min.z<=AABB.Min.z) and
         (InAABB.Max.x>=AABB.Min.x) and (InAABB.Max.y>=AABB.Min.y) and (InAABB.Max.z>=AABB.Min.z) and
         (InAABB.Min.x<=AABB.Max.x) and (InAABB.Min.y<=AABB.Max.y) and (InAABB.Min.z<=AABB.Max.z) and
         (InAABB.Max.x>=AABB.Max.x) and (InAABB.Max.y>=AABB.Max.y) and (InAABB.Max.z>=AABB.Max.z);
end;

function AABBContains(const AABB:TPhysicsAABB;Vector:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=((Vector.x>=AABB.Min.x) and (Vector.x<=AABB.Max.x)) and
         ((Vector.y>=AABB.Min.y) and (Vector.y<=AABB.Max.y)) and
         ((Vector.z>=AABB.Min.z) and (Vector.z<=AABB.Max.z));
end;

function AABBContainsEx(const InAABB,AABB:TPhysicsAABB):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=((InAABB.Min.x-AABBEPSILON)<=(AABB.Min.x+AABBEPSILON)) and ((InAABB.Min.y-AABBEPSILON)<=(AABB.Min.y+AABBEPSILON)) and ((InAABB.Min.z-AABBEPSILON)<=(AABB.Min.z+AABBEPSILON)) and
         ((InAABB.Max.x+AABBEPSILON)>=(AABB.Min.x+AABBEPSILON)) and ((InAABB.Max.y+AABBEPSILON)>=(AABB.Min.y+AABBEPSILON)) and ((InAABB.Max.z+AABBEPSILON)>=(AABB.Min.z+AABBEPSILON)) and
         ((InAABB.Min.x-AABBEPSILON)<=(AABB.Max.x-AABBEPSILON)) and ((InAABB.Min.y-AABBEPSILON)<=(AABB.Max.y-AABBEPSILON)) and ((InAABB.Min.z-AABBEPSILON)<=(AABB.Max.z-AABBEPSILON)) and
         ((InAABB.Max.x+AABBEPSILON)>=(AABB.Max.x-AABBEPSILON)) and ((InAABB.Max.y+AABBEPSILON)>=(AABB.Max.y-AABBEPSILON)) and ((InAABB.Max.z+AABBEPSILON)>=(AABB.Max.z-AABBEPSILON));
end;

function AABBContainsEx(const AABB:TPhysicsAABB;Vector:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 result:=((Vector.x>=(AABB.Min.x-AABBEPSILON)) and (Vector.x<=(AABB.Max.x+AABBEPSILON))) and
         ((Vector.y>=(AABB.Min.y-AABBEPSILON)) and (Vector.y<=(AABB.Max.y+AABBEPSILON))) and
         ((Vector.z>=(AABB.Min.z-AABBEPSILON)) and (Vector.z<=(AABB.Max.z+AABBEPSILON)));
end;

function AABBGetIntersectAABB(const AABB,WithAABB:TPhysicsAABB):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if AABBIntersectEx(AABB,WithAABB) then begin
  result.Min.x:=Max(AABB.Min.x-EPSILON,WithAABB.Min.x-EPSILON);
  result.Min.y:=Max(AABB.Min.y-EPSILON,WithAABB.Min.y-EPSILON);
  result.Min.z:=Max(AABB.Min.z-EPSILON,WithAABB.Min.z-EPSILON);
  result.Max.x:=Min(AABB.Max.x+EPSILON,WithAABB.Max.x+EPSILON);
  result.Max.y:=Min(AABB.Max.y+EPSILON,WithAABB.Max.y+EPSILON);
  result.Max.z:=Min(AABB.Max.z+EPSILON,WithAABB.Max.z+EPSILON);
 end else begin
  fillchar(result,sizeof(TPhysicsAABB),#0);
 end;
end;

function AABBRayIntersect(const AABB:TPhysicsAABB;const Origin,Direction:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Center,BoxExtents,Diff:TPhysicsVector3;
begin
 result:=false;
 Center:=Vector3ScalarMul(Vector3Add(AABB.Min,AABB.Max),0.5);
 BoxExtents:=Vector3Sub(AABB.Max,AABB.Min);
 Diff:=Vector3Sub(Origin,Center);
 if (((abs(Diff.x)>BoxExtents.x) and ((Diff.x*Direction.x)>=0)) or
     ((abs(Diff.y)>BoxExtents.y) and ((Diff.y*Direction.y)>=0)) or
     ((abs(Diff.z)>BoxExtents.z) and ((Diff.z*Direction.z)>=0))) or
    ((abs((Direction.y*Diff.z)-(Direction.z*Diff.y))>((BoxExtents.y*abs(Direction.z))+(BoxExtents.z*abs(Direction.y)))) or
     (abs((Direction.z*Diff.x)-(Direction.x*Diff.z))>((BoxExtents.x*abs(Direction.z))+(BoxExtents.z*abs(Direction.x)))) or
     (abs((Direction.x*Diff.y)-(Direction.y*Diff.x))>((BoxExtents.x*abs(Direction.y))+(BoxExtents.y*abs(Direction.x))))) then exit;
 result:=true;
end;

function AABBTransform(const DstAABB:TPhysicsAABB;const Transform:TPhysicsMatrix4x4):TPhysicsAABB; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j:integer;
    a,b:TPhysicsFloat;
begin
 result.min:=Vector3(Transform[3,0],Transform[3,1],Transform[3,2]);
 result.max:=result.min;
 for i:=0 to 2 do begin
  for j:=0 to 2 do begin
   a:=Transform[j,i]*DstAABB.min.xyz[j];
   b:=Transform[j,i]*DstAABB.max.xyz[j];
   if a<b then begin
    result.min.xyz[i]:=result.min.xyz[i]+a;
    result.max.xyz[i]:=result.max.xyz[i]+b;
   end else begin
    result.min.xyz[i]:=result.min.xyz[i]+b;
    result.max.xyz[i]:=result.max.xyz[i]+a;
   end;
  end;
 end;
end;

function CalculateArea(const v0,v1,v2:TPhysicsVector3):TPhysicsFloat;
begin
 result:=Vector3LengthSquared(Vector3Cross(Vector3Sub(v1,v0),Vector3Sub(v2,v0)));
end;

function CalculateVolume(const v0,v1,v2,v3:TPhysicsVector3):TPhysicsFloat;
var a,b,c:TPhysicsVector3;
begin
 a:=Vector3Sub(v0,v3);
 b:=Vector3Sub(v1,v3);
 c:=Vector3Sub(v2,v3);
 result:=(a.x*((b.z*c.y)-(b.y*c.z)))+(a.y*((b.x*c.z)-(b.z*c.x)))+(a.z*((b.y*c.x)-(b.x*c.y)));
end;

function SegmentTriangleIntersection(var tS,tT0,tT1:TPhysicsFloat;seg:TPhysicsSegment;triangle:TPhysicsSegmentTriangle):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var u,v,t,a,f:TPhysicsFloat;
    e1,e2,p,s,q:TPhysicsVector3;
begin
 result:=false;
 tS:=0;
 tT0:=0;
 tT1:=0;
 e1:=triangle.Edge0;
 e2:=triangle.Edge1;
 p:=Vector3Cross(seg.Delta,e2);
 a:=Vector3Dot(e1,p);
 if abs(a)<EPSILON then exit;
 f:=1.0/a;
 s:=Vector3Sub(seg.Origin,triangle.Origin);
 u:=f*Vector3Dot(s,p);
 if (u<0.0) or (u>1.0) then exit;
 q:=Vector3Cross(s,e1);
 v:=f*Vector3Dot(seg.Delta,q);
 if (v<0.0) or ((u+v)>1.0) then exit;
 t:=f*Vector3Dot(e2,q);
 if (t<0.0) or (t>1.0) then exit;
 tS:=t;
 tT0:=u;
 tT1:=v;
 result:=false;
end;

function SegmentSegmentDistanceSq(var t0,t1:TPhysicsFloat;seg0,seg1:TPhysicsSegment):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var kDiff:TPhysicsVector3;
    fA00,fA01,fA11,fB0,fC,fDet,fB1,fS,fT,fSqrDist,fTmp,fInvDet:TPhysicsFloat;
begin
 kDiff:=Vector3Sub(seg0.Origin,seg1.Origin);
 fA00:=Vector3LengthSquared(seg0.Delta);
 fA01:=-Vector3Dot(seg0.Delta,seg1.Delta);
 fA11:=Vector3LengthSquared(seg1.Delta);
 fB0:=Vector3Dot(kDiff,seg0.Delta);
 fC:=Vector3LengthSquared(kDiff);
 fDet:=abs((fA00*fA11)-(fA01*fA01));
 if fDet>=EPSILON then begin
  // line segments are not parallel
  fB1:=-Vector3Dot(kDiff,seg1.Delta);
  fS:=(fA01*fB1)-(fA11*fB0);
  fT:=(fA01*fB0)-(fA00*fB1);
  if fS>=0.0 then begin
   if fS<=fDet then begin
    if fT>=0.0 then begin
     if fT<=fDet then begin // region 0 (interior)
      // minimum at two interior points of 3D lines
      fInvDet:=1.0/fDet;
      fS:=fS*fInvDet;
      fT:=fT*fInvDet;
      fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
     end else begin // region 3 (side)
      fT:=1.0;
      fTmp:=fA01+fB0;
      if fTmp>=0.0 then begin
       fS:=0.0;
       fSqrDist:=fA11+(2.0*fB1)+fC;
      end else if (-fTmp)>=fA00 then begin
       fS:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB1+fTmp));
      end else begin
       fS:=-fTmp/fA00;
       fSqrDist:=fTmp*fS+fA11+(2.0*fB1)+fC;
      end;
     end;
    end else begin // region 7 (side)
     fT:=0.0;
     if fB0>=0.0 then begin
      fS:=0.0;
      fSqrDist:=fC;
     end else if (-fB0)>=fA00 then begin
      fS:=1.0;
      fSqrDist:=fA00+(2.0*fB0)+fC;
     end else begin
      fS:=(-fB0)/fA00;
      fSqrDist:=(fB0*fS)+fC;
     end;
    end;
   end else begin
    if fT>=0.0 then begin
     if fT<=fDet then begin // region 1 (side)
      fS:=1.0;
      fTmp:=fA01+fB1;
      if fTmp>=0.0 then begin
       fT:=0.0;
       fSqrDist:=fA00+(2.0*fB0)+fC;
      end else if (-fTmp)>=fA11 then begin
       fT:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB0+fTmp));
      end else begin
       fT:=(-fTmp)/fA11;
       fSqrDist:=(fTmp*fT)+fA00+(2.0*fB0)+fC;
      end;
     end else begin // region 2 (corner)
      fTmp:=fA01+fB0;
      if (-fTmp)<=fA00 then begin
       fT:=1.0;
       if fTmp>=0.0 then begin
        fS:=0.0;
        fSqrDist:=fA11+(2.0*fB1)+fC;
       end else begin
        fS:=(-fTmp)/fA00;
        fSqrDist:=(fTmp*fS)+fA11+(2.0*fB1)+fC;
       end;
      end else begin
       fS:=1.0;
       fTmp:=fA01+fB1;
       if fTmp>=0.0 then begin
        fT:=0.0;
        fSqrDist:=fA00+(2.0*fB0)+fC;
       end else if (-fTmp)>=fA11 then begin
        fT:=1.0;
        fSqrDist:=fA00+fA11+fC+(2.0*(fB0+fTmp));
       end else begin
        fT:=(-fTmp)/fA11;
        fSqrDist:=(fTmp*fT)+fA00+(2.0*fB0)+fC;
       end;
      end;
     end;
    end else begin // region 8 (corner)
     if (-fB0)<fA00 then begin
      fT:=0.0;
      if fB0>=0.0 then begin
       fS:=0.0;
       fSqrDist:=fC;
      end else begin
       fS:=(-fB0)/fA00;
       fSqrDist:=(fB0*fS)+fC;
      end;
     end else begin
      fS:=1.0;
      fTmp:=fA01+fB1;
      if fTmp>=0.0 then begin
       fT:=0.0;
       fSqrDist:=fA00+(2.0*fB0)+fC;
      end else if (-fTmp)>=fA11 then begin
       fT:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB0+fTmp));
      end else begin
       fT:=(-fTmp)/fA11;
       fSqrDist:=(fTmp*fT)+fA00+(2.0*fB0)+fC;
      end;
      end;
    end;
   end;
  end else begin
   if fT>=0.0 then begin
    if fT<=fDet then begin // region 5 (side)
     fS:=0.0;
     if fB1>=0.0 then begin
      fT:=0.0;
      fSqrDist:=fC;
     end else if (-fB1)>=fA11 then begin
      fT:=1.0;
      fSqrDist:=fA11+(2.0*fB1)+fC;
     end else begin
      fT:=(-fB1)/fA11;
      fSqrDist:=fB1*fT+fC;
     end
    end else begin // region 4 (corner)
     fTmp:=fA01+fB0;
     if fTmp<0.0 then begin
      fT:=1.0;
      if (-fTmp)>=fA00 then begin
       fS:=1.0;
       fSqrDist:=fA00+fA11+fC+(2.0*(fB1+fTmp));
      end else begin
       fS:=(-fTmp)/fA00;
       fSqrDist:=fTmp*fS+fA11+(2.0*fB1)+fC;
      end;
     end else begin
      fS:=0.0;
      if fB1>=0.0 then begin
       fT:=0.0;
       fSqrDist:=fC;
      end else if (-fB1)>=fA11 then begin
       fT:=1.0;
       fSqrDist:=fA11+(2.0*fB1)+fC;
      end else begin
       fT:=(-fB1)/fA11;
       fSqrDist:=(fB1*fT)+fC;
      end;
     end;
    end;
   end else begin // region 6 (corner)
    if fB0<0.0 then begin
     fT:=0.0;
     if (-fB0)>=fA00 then begin
      fS:=1.0;
      fSqrDist:=fA00+(2.0*fB0)+fC;
     end else begin
      fS:=(-fB0)/fA00;
      fSqrDist:=(fB0*fS)+fC;
     end;
    end else begin
     fS:=0.0;
     if fB1>=0.0 then begin
      fT:=0.0;
      fSqrDist:=fC;
     end else if (-fB1)>=fA11 then begin
      fT:=1.0;
      fSqrDist:=fA11+(2.0*fB1)+fC;
     end else begin
      fT:=(-fB1)/fA11;
      fSqrDist:=(fB1*fT)+fC;
     end;
    end;
   end;
  end;
 end else begin // line segments are parallel
  if fA01>0.0 then begin // direction vectors form an obtuse angle
   if fB0>=0.0 then begin
    fS:=0.0;
    fT:=0.0;
    fSqrDist:=fC;
   end else if (-fB0)<=fA00 then begin
    fS:=(-fB0)/fA00;
    fT:=0.0;
    fSqrDist:=(fB0*fS)+fC;
   end else begin
    fB1:=-Vector3Dot(kDiff,seg1.Delta);
    fS:=1.0;
    fTmp:=fA00+fB0;
    if (-fTmp)>=fA01 then begin
     fT:=1.0;
     fSqrDist:=fA00+fA11+fC+(2.0*(fA01+fB0+fB1));
    end else begin
     fT:=(-fTmp)/fA01;
     fSqrDist:=fA00+(2.0*fB0)+fC+(fT*((fA11*fT)+(2.0*(fA01+fB1))));
    end;
   end;
  end else begin // direction vectors form an acute angle
   if (-fB0)>=fA00 then begin
    fS:=1.0;
    fT:=0.0;
    fSqrDist:=fA00+(2.0*fB0)+fC;
   end else if fB0<=0.0 then begin
    fS:=(-fB0)/fA00;
    fT:=0.0;
    fSqrDist:=(fB0*fS)+fC;
   end else begin
    fB1:=-Vector3Dot(kDiff,seg1.Delta);
    fS:=0.0;
    if fB0>=(-fA01) then begin
     fT:=1.0;
     fSqrDist:=fA11+(2.0*fB1)+fC;
    end else begin
     fT:=(-fB0)/fA01;
     fSqrDist:=fC+(fT*((2.0)*fB1)+(fA11*fT));
    end;
   end;
  end;
 end;
 t0:=fS;
 t1:=fT;
 result:=abs(fSqrDist);
end;

function PointTriangleDistanceSq(var pfSParam,pfTParam:TPhysicsFloat;rkPoint:TPhysicsVector3;rkTri:TPhysicsSegmentTriangle):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var kDiff:TPhysicsVector3;
    fA00,fA01,fA11,fB0,fC,fDet,fB1,fS,fT,fSqrDist,fInvDet,
    fTmp0,fTmp1,fNumer,fDenom:TPhysicsFloat;
begin
 kDiff:=Vector3Sub(rkTri.Origin,rkPoint);
 fA00:=Vector3LengthSquared(rkTri.Edge0);
 fA01:=Vector3Dot(rkTri.Edge0,rkTri.Edge1);
 fA11:=Vector3LengthSquared(rkTri.Edge1);
 fB0:=Vector3Dot(kDiff,rkTri.Edge0);
 fB1:=Vector3Dot(kDiff,rkTri.Edge1);
 fC:=Vector3LengthSquared(kDiff);
 fDet:=max(abs((fA00*fA11)-(fA01*fA01)),EPSILON);
 fS:=(fA01*fB1)-(fA11*fB0);
 fT:=(fA01*fB0)-(fA00*fB1);
 if (fS+fT)<=fDet then begin
  if fS<0.0 then begin
   if fT<0.0 then begin // region 4
    if fB0<0.0 then begin
     fT:=0.0;
     if (-fB0)>=fA00 then begin
      fS:=1.0;
      fSqrDist:=fA00+(2.0*fB0)+fC;
     end else begin
      fS:=(-fB0)/fA00;
      fSqrDist:=(fB0*fS)+fC;
     end;
    end else begin
     fS:=0.0;
     if fB1>=0.0 then begin
      fT:=0.0;
      fSqrDist:=fC;
     end else if (-fB1)>=fA11 then begin
      fT:=1.0;
      fSqrDist:=fA11+(2.0*fB1)+fC;
     end else begin
      fT:=(-fB1)/fA11;
      fSqrDist:=(fB1*fT)+fC;
     end;
    end;
   end else begin // region 3
    fS:=0.0;
    if fB1>=0.0 then begin
     fT:=0.0;
     fSqrDist:=fC;
    end else if (-fB1)>=fA11 then begin
     fT:=1.0;
     fSqrDist:=fA11+(2.0*fB1)+fC;
    end else begin
     fT:=(-fB1)/fA11;
     fSqrDist:=(fB1*fT)+fC;
    end;
   end;
  end else if fT<0.0 then begin // region 5
   fT:=0.0;
   if fB0>=0.0 then begin
    fS:=0.0;
    fSqrDist:=fC;
   end else if (-fB0)>=fA00 then begin
    fS:=1.0;
    fSqrDist:=fA00+(2.0*fB0)+fC;
   end else begin
    fS:=(-fB0)/fA00;
    fSqrDist:=(fB0*fS)+fC;
   end;
  end else begin // region 0
   // minimum at interior point
   fInvDet:=1.0/fDet;
   fS:=fS*fInvDet;
   fT:=fT*fInvDet;
   fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
  end;
 end else begin
  if fS<0.0 then begin // region 2
   fTmp0:=fA01+fB0;
   fTmp1:=fA11+fB1;
   if fTmp1>fTmp0 then begin
    fNumer:=fTmp1-fTmp0;
    fDenom:=fA00-(2.0*fA01)+fA11;
    if fNumer>=fDenom then begin
     fS:=1.0;
     fT:=0.0;
     fSqrDist:=fA00+(2.0*fB0)+fC;
    end else begin
     fS:=fNumer/fDenom;
     fT:=1.0-fS;
     fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2*fB1)))+fC;
    end;
   end else begin
    fS:=0.0;
    if fTmp1<=0.0 then begin
     fT:=1.0;
     fSqrDist:=fA11+(2.0*fB1)+fC;
    end else if fB1>=0.0 then begin
     fT:=0.0;
     fSqrDist:=fC;
    end else begin
     fT:=(-fB1)/fA11;
     fSqrDist:=(fB1*fT)+fC;
    end;
   end;
  end else if fT<0.0 then begin // region 6
   fTmp0:=fA01+fB1;
   fTmp1:=fA00+fB0;
   if fTmp1>fTmp0 then begin
    fNumer:=fTmp1-fTmp0;
    fDenom:=fA00-(2.0*fA01)+fA11;
    if fNumer>=fDenom then begin
     fT:=1.0;
     fS:=0.0;
     fSqrDist:=fA11+(2*fB1)+fC;
    end else begin
     fT:=fNumer/fDenom;
     fS:=1.0-fT;
     fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
    end;
   end else begin
    fT:=0.0;
    if fTmp1<=0.0 then begin
     fS:=1.0;
     fSqrDist:=fA00+(2.0*fB0)+fC;
    end else if fB0>=0.0 then begin
     fS:=0.0;
     fSqrDist:=fC;
    end else begin
     fS:=(-fB0)/fA00;
     fSqrDist:=(fB0*fS)+fC;
    end;
   end;
  end else begin // region 1
   fNumer:=((fA11+fB1)-fA01)-fB0;
   if fNumer<=0.0 then begin
    fS:=0.0;
    fT:=1.0;
    fSqrDist:=fA11+(2.0*fB1)+fC;
   end else begin
    fDenom:=fA00-(2.0*fA01)+fA11;
    if fNumer>=fDenom then begin
     fS:=1.0;
     fT:=0.0;
     fSqrDist:=fA00+(2.0*fB0)+fC;
    end else begin
     fS:=fNumer/fDenom;
     fT:=1.0-fS;
     fSqrDist:=(fS*((fA00*fS)+(fA01*fT)+(2.0*fB0)))+(fT*((fA01*fS)+(fA11*fT)+(2.0*fB1)))+fC;
    end;
   end;
  end;
 end;
 pfSParam:=fS;
 pfTParam:=fT;
 result:=abs(fSqrDist);
end;

function SegmentTriangleDistanceSq(var segT,triT0,triT1:TPhysicsFloat;seg:TPhysicsSegment;triangle:TPhysicsSegmentTriangle):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var s,t,u,distEdgeSq,startTriSq,endTriSq:TPhysicsFloat;
    tseg:TPhysicsSegment;
begin
 result:=INFINITY;
 if SegmentTriangleIntersection(segT,triT0,triT1,seg,triangle) then begin
  segT:=0;
  triT0:=0;
  triT1:=0;
  result:=0;
  exit;
 end;
 tseg.Origin:=triangle.Origin;
 tseg.Delta:=triangle.Edge0;
 distEdgeSq:=SegmentSegmentDistanceSq(s,t,seg,tseg);
 if distEdgeSq<result then begin
  result:=distEdgeSq;
  segT:=s;
  triT0:=t;
  triT1:=0.0;
 end;
 tseg.Delta:=triangle.Edge1;
 distEdgeSq:=SegmentSegmentDistanceSq(s,t,seg,tseg);
 if distEdgeSq<result then begin
  result:=distEdgeSq;
  segT:=s;
  triT0:=0.0;
  triT1:=t;
 end;
 tseg.Origin:=Vector3Add(triangle.Origin,triangle.Edge1);
 tseg.Delta:=triangle.Edge2;
 distEdgeSq:=SegmentSegmentDistanceSq(s,t,seg,tseg);
 if distEdgeSq<result then begin
  result:=distEdgeSq;
  segT:=s;
  triT0:=1.0-t;
  triT1:=t;
 end;
 startTriSq:=PointTriangleDistanceSq(t,u,seg.Origin,triangle);
 if startTriSq<result then begin
  result:=startTriSq;
  segT:=0.0;
  triT0:=t;
  triT1:=u;
 end;
 endTriSq:=PointTriangleDistanceSq(t,u,Vector3Add(seg.Origin,seg.Delta),triangle);
 if endTriSq<result then begin
  result:=endTriSq;
  segT:=1.0;
  triT0:=t;
  triT1:=u;
 end;
end;

function BoxGetDistanceToPoint(Point:TPhysicsVector3;const Center,Size:TPhysicsVector3;const InvTransformMatrix,TransformMatrix:TPhysicsMatrix4x4;var ClosestBoxPoint:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var HalfSize:TPhysicsVector3;
begin
 result:=0;
 ClosestBoxPoint:=Vector3Sub(Vector3TermMatrixMul(Point,InvTransformMatrix),Center);
 HalfSize.x:=abs(Size.x*0.5);
 HalfSize.y:=abs(Size.y*0.5);
 HalfSize.z:=abs(Size.z*0.5);
 if ClosestBoxPoint.x<-HalfSize.x then begin
  result:=result+sqr(ClosestBoxPoint.x-(-HalfSize.x));
  ClosestBoxPoint.x:=-HalfSize.x;
 end else if ClosestBoxPoint.x>HalfSize.x then begin
  result:=result+sqr(ClosestBoxPoint.x-HalfSize.x);
  ClosestBoxPoint.x:=HalfSize.x;
 end;
 if ClosestBoxPoint.y<-HalfSize.y then begin
  result:=result+sqr(ClosestBoxPoint.y-(-HalfSize.y));
  ClosestBoxPoint.y:=-HalfSize.y;
 end else if ClosestBoxPoint.y>HalfSize.y then begin
  result:=result+sqr(ClosestBoxPoint.y-HalfSize.y);
  ClosestBoxPoint.y:=HalfSize.y;
 end;
 if ClosestBoxPoint.z<-HalfSize.z then begin
  result:=result+sqr(ClosestBoxPoint.z-(-HalfSize.z));
  ClosestBoxPoint.z:=-HalfSize.z;
 end else if ClosestBoxPoint.z>HalfSize.z then begin
  result:=result+sqr(ClosestBoxPoint.z-HalfSize.z);
  ClosestBoxPoint.z:=HalfSize.z;
 end;
 ClosestBoxPoint:=Vector3TermMatrixMul(Vector3Add(ClosestBoxPoint,Center),TransformMatrix);
end;

function GetDistanceFromLine(const s:TPhysicsVector3;var project:TPhysicsVector3;const pointa,pointb:TPhysicsVector3):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ba,pa:TPhysicsVector3;
    len,k:TPhysicsFloat;
begin
 ba:=Vector3Sub(pointb,pointa);
 len:=Vector3Length(ba);
 if len<EPSILON then begin
  ba:=Vector3Origin;
 end else begin
  ba:=Vector3ScalarMul(ba,1/len);
 end;
 pa:=Vector3Sub(s,pointa);
 k:=Vector3Dot(pa,ba);
 project:=Vector3Add(pointa,Vector3ScalarMul(ba,k));
 result:=Vector3Length(Vector3Sub(s,project));
end;

procedure LineClosestApproach(const pa,ua,pb,ub:TPhysicsVector3;var Alpha,Beta:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var p:TPhysicsVector3;
    uaub,q1,q2,d:TPhysicsFloat;
begin
 p:=Vector3Sub(pb,pa);
 uaub:=Vector3Dot(ua,ub);
 q1:=Vector3Dot(ua,p);
 q2:=Vector3Dot(ub,p);
 d:=1.0-sqr(uaub);
 if d<EPSILON then begin
  Alpha:=0;
  Beta:=0;
 end else begin
  d:=1/d;
  Alpha:=(q1+(uaub*q2))*d;
  Beta:=((uaub*q1)+q2)*d;
 end;
end;

procedure ClosestLineBoxPoints(const p1,p2,c:TPhysicsVector3;const ir,r:TPhysicsMatrix4x4;const side:TPhysicsVector3;var lret,bret:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const tanchorepsilon:TPhysicsFloat={$ifdef physicsdouble}1e-307{$else}1e-19{$endif};
var tmp,s,v,sign,v2,h:TPhysicsVector3;
    region:array[0..2] of integer;
    tanchor:array[0..2] of TPhysicsFloat;
    i:integer;
    t,dd2dt,nextt,nextdd2dt:TPhysicsFloat;
    DoGetAnswer:boolean;
begin
 s:=Vector3TermMatrixMul(Vector3Sub(p1,c),ir);
 v:=Vector3TermMatrixMul(Vector3Sub(p2,p1),ir);
 for i:=0 to 2 do begin
  if v.xyz[i]<0 then begin
   s.xyz[i]:=-s.xyz[i];
   v.xyz[i]:=-v.xyz[i];
   sign.xyz[i]:=-1;
  end else begin
   sign.xyz[i]:=1;
  end;
 end;
 v2:=Vector3Mul(v,v);
 h:=Vector3ScalarMul(side,0.5);
 for i:=0 to 2 do begin
  if v.xyz[i]>tanchorepsilon then begin
   if s.xyz[i]<-h.xyz[i] then begin
    region[i]:=-1;
    tanchor[i]:=((-h.xyz[i])-s.xyz[i])/v.xyz[i];
   end else begin
    if s.xyz[i]>h.xyz[i] then begin
     region[i]:=1;
    end else begin
     region[i]:=0;
    end;
    tanchor[i]:=(h.xyz[i]-s.xyz[i])/v.xyz[i];
   end;
  end else begin
   region[i]:=0;
   tanchor[i]:=2;
  end;
 end;
 t:=0;
 dd2dt:=0;
 for i:=0 to 2 do begin
  if region[i]<>0 then begin
   dd2dt:=dd2dt-(v2.xyz[i]*tanchor[i]);
  end;
 end;
 if dd2dt<0 then begin
  DoGetAnswer:=false;
  repeat
   nextt:=1;
   for i:=0 to 2 do begin
    if (tanchor[i]>t) and (tanchor[i]<1) and (tanchor[i]<nextt) then begin
     nextt:=tanchor[i];
    end;
   end;
   nextdd2dt:=0;
   for i:=0 to 2 do begin
    if region[i]<>0 then begin
     nextdd2dt:=nextdd2dt+(v2.xyz[i]*(nextt-tanchor[i]));
    end;
   end;
   if nextdd2dt>=0 then begin
    t:=t-(dd2dt/((nextdd2dt-dd2dt)/(nextt-t)));
    DoGetAnswer:=true;
    break;
   end;
   for i:=0 to 2 do begin
    if abs(tanchor[i]-nextt)<EPSILON then begin
     tanchor[i]:=(h.xyz[i]-s.xyz[i])/v.xyz[i];
     inc(region[i]);
    end;
   end;
   t:=nextt;
   dd2dt:=nextdd2dt;
  until t>=1;
  if not DoGetAnswer then begin
   t:=1;
  end;
 end;
 lret:=Vector3Add(p1,Vector3ScalarMul(Vector3Sub(p2,p1),t));
 for i:=0 to 2 do begin
  tmp.xyz[i]:=sign.xyz[i]*(s.xyz[i]+(t*v.xyz[i]));
  if tmp.xyz[i]<-h.xyz[i] then begin
   tmp.xyz[i]:=-h.xyz[i];
  end else if tmp.xyz[i]>h.xyz[i] then begin
   tmp.xyz[i]:=h.xyz[i];
  end;
 end;
 bret:=Vector3Add(c,Vector3TermMatrixMul(tmp,r));
end;

procedure ClosestLineSegmentPoints(const a1,a2,b1,b2:TPhysicsVector3;var cp1,cp2:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var a1a2,b1b2,a1b1,a1b2,a2b1,a2b2,n:TPhysicsVector3;
    la,lb,k,da1,da2,da3,da4,db1,db2,db3,db4,det,Alpha,Beta:TPhysicsFloat;
begin
 a1a2:=Vector3Sub(a2,a1);
 b1b2:=Vector3Sub(b2,b1);
 a1b1:=Vector3Sub(b1,a1);
 da1:=Vector3Dot(a1a2,a1b1);
 db1:=Vector3Dot(b1b2,a1b1);
 if (da1<=0) and (db1>=0) then begin
  cp1:=a1;
  cp2:=b1;
  exit;
 end;
 a1b2:=Vector3Sub(b2,a1);
 da2:=Vector3Dot(a1a2,a1b2);
 db2:=Vector3Dot(b1b2,a1b2);
 if (da2<=0) and (db2<=0) then begin
  cp1:=a1;
  cp2:=b2;
  exit;
 end;
 a2b1:=Vector3Sub(b1,a2);
 da3:=Vector3Dot(a1a2,a2b1);
 db3:=Vector3Dot(b1b2,a2b1);
 if (da3>=0) and (db3>=0) then begin
  cp1:=a2;
  cp2:=b1;
  exit;
 end;
 a2b2:=Vector3Sub(b2,a2);
 da4:=Vector3Dot(a1a2,a2b2);
 db4:=Vector3Dot(b1b2,a2b2);
 if (da4>=0) and (db4<=0) then begin
  cp1:=a2;
  cp2:=b2;     
  exit;
 end;
 la:=Vector3Dot(a1a2,a1a2);
 if (da1>=0) and (da3<=0) then begin
  k:=da1/la;
  n:=Vector3Sub(a1b1,Vector3ScalarMul(a1a2,k));
  if Vector3Dot(b1b2,n)>=0 then begin
   cp1:=Vector3Add(a1,Vector3ScalarMul(a1a2,k));
   cp2:=b1;
   exit;
  end;
 end;
 if (da2>=0) and (da4<=0) then begin
  k:=da2/la;
  n:=Vector3Sub(a1b2,Vector3ScalarMul(a1a2,k));
  if Vector3Dot(b1b2,n)<=0 then begin
   cp1:=Vector3Add(a1,Vector3ScalarMul(a1a2,k));
   cp2:=b2;
   exit;
  end;
 end;
 lb:=Vector3Dot(b1b2,b1b2);
 if (db1<=0) and (db2>=0) then begin
  k:=-db1/lb;
  n:=Vector3Sub(Vector3Neg(a1a2),Vector3ScalarMul(b1b2,k));
  if Vector3Dot(a1a2,n)>=0 then begin
   cp1:=a1;
   cp2:=Vector3Add(b1,Vector3ScalarMul(b1b2,k));
   exit;
  end;
 end;
 if (db3<=0) and (db4>=0) then begin
  k:=-db3/lb;
  n:=Vector3Sub(Vector3Neg(a2b1),Vector3ScalarMul(b1b2,k));
  if Vector3Dot(a1a2,n)>=0 then begin
   cp1:=a2;
   cp2:=Vector3Add(b1,Vector3ScalarMul(b1b2,k));
   exit;
  end;
 end;
 k:=Vector3Dot(a1a2,b1b2);
 det:=(la*lb)-sqr(k);
 if det<=EPSILON then begin
  cp1:=a1;
  cp2:=b1;
 end else begin
  det:=1/det;
  Alpha:=((lb*da1)-(k*db1))*det;
  Beta:=((k*da1)-(la*db1))*det;
  cp1:=Vector3Add(a1,Vector3ScalarMul(a1a2,Alpha));
  cp2:=Vector3Add(b1,Vector3ScalarMul(b1b2,Beta));
 end;
end;

function IntLog2(x:longword):longword; {$ifdef cpu386}register;
asm
 test eax,eax
 jz @Done
 bsr eax,eax
 @Done:
end;
{$else}
begin
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
 x:=x shr 1;
 x:=x-((x shr 1) and $55555555);
 x:=((x shr 2) and $33333333)+(x and $33333333);
 x:=((x shr 4)+x) and $0f0f0f0f;
 x:=x+(x shr 8);
 x:=x+(x shr 16);
 result:=x and $3f;
end;
{$endif}

procedure PhysicsSort(DataBase:pointer;Count:integer;CompareFunction:TPhysicsSortCompareFunction); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
type PPointers=^TPointers;
     TPointers=array[0..0] of pointer;
 procedure ProcessSort(Left,Right:integer;Depth:longword);
  procedure SiftDown(Current,MaxIndex:integer);
  var SiftLeft,SiftRight,Largest:Integer;
      t:pointer;
  begin
   SiftLeft:=Left+(2*(Current-Left))+1;
   SiftRight:=Left+(2*(Current-Left))+2;
   Largest:=Current;
   if (SiftLeft<=MaxIndex) and (CompareFunction(PPointers(DataBase)^[SiftLeft],PPointers(DataBase)^[Largest])>0) then begin
    Largest:=SiftLeft;
   end;
   if (SiftRight<=MaxIndex) and (CompareFunction(PPointers(DataBase)^[SiftRight],PPointers(DataBase)^[Largest])>0) then begin
    Largest:=SiftRight;
   end;
   if Largest<>Current then begin
    t:=PPointers(DataBase)^[Current];
    PPointers(DataBase)^[Current]:=PPointers(DataBase)^[Largest];
    PPointers(DataBase)^[Largest]:=t;
    SiftDown(Largest,MaxIndex);
   end;
  end;
 var Middle,i,j:integer;
     x,t:pointer;
 begin
  if Left>Right then begin
   exit;
  end;
  if (Right-Left)<16 then begin
   // Insertion sort
   for i:=Left+1 to Right do begin
    t:=PPointers(DataBase)^[i];
    j:=i-1;
    while (j>=Left) and (CompareFunction(t,PPointers(DataBase)^[j])<0) do begin
     PPointers(DataBase)^[j+1]:=PPointers(DataBase)^[j];
     dec(j);
    end;
    PPointers(DataBase)^[j+1]:=t;
   end;
  end else if Depth=0 then begin
   // Heap sort
   for i:=((Left+Right+1) div 2)-1 downto Left do begin
    SiftDown(i,Right);
   end;
   for i:=Right downto Left+1 do begin
    t:=PPointers(DataBase)^[i];
    PPointers(DataBase)^[i]:=PPointers(DataBase)^[Left];
    PPointers(DataBase)^[Left]:=t;
    SiftDown(Left,i-1);
   end;
  end else begin
   // Quick sort with median of three
   Middle:=(Left+Right) div 2;
   if CompareFunction(PPointers(DataBase)^[Left],PPointers(DataBase)^[Middle])>0 then begin
    t:=PPointers(DataBase)^[Left];
    PPointers(DataBase)^[Left]:=PPointers(DataBase)^[Middle];
    PPointers(DataBase)^[Middle]:=t;
   end;
   if CompareFunction(PPointers(DataBase)^[Left],PPointers(DataBase)^[Right])>0 then begin
    t:=PPointers(DataBase)^[Left];
    PPointers(DataBase)^[Left]:=PPointers(DataBase)^[Right];
    PPointers(DataBase)^[Right]:=t;
   end;
   if CompareFunction(PPointers(DataBase)^[Middle],PPointers(DataBase)^[Right])>0 then begin
    t:=PPointers(DataBase)^[Middle];
    PPointers(DataBase)^[Middle]:=PPointers(DataBase)^[Right];
    PPointers(DataBase)^[Right]:=t;
   end;
   t:=PPointers(DataBase)^[Middle];
   PPointers(DataBase)^[Middle]:=PPointers(DataBase)^[Right-1];
   PPointers(DataBase)^[Right-1]:=t;
   x:=t;
   i:=Left;                           
   j:=Right-1;
   while true do begin
    repeat
     inc(i);
    until not ((i<Right) and (CompareFunction(PPointers(DataBase)^[i],x)<0));
    repeat
     dec(j);
    until not ((j>Left) and (CompareFunction(PPointers(DataBase)^[j],x)>0));
    if i>=j then begin
     break;
    end else begin
     t:=PPointers(DataBase)^[i];
     PPointers(DataBase)^[i]:=PPointers(DataBase)^[j];
     PPointers(DataBase)^[j]:=t;
    end;
   end;
   t:=PPointers(DataBase)^[i];
   PPointers(DataBase)^[i]:=PPointers(DataBase)^[Right-1];
   PPointers(DataBase)^[Right-1]:=t;
   ProcessSort(Left,i-1,Depth-1);
   ProcessSort(i+1,Right,Depth-1);
  end;
 end;
{$ifdef physicssortdebug}
 procedure ProcessBeRoSort(Left,Right:integer);
 var i:integer;
     t:pointer;
 begin
  i:=Left;
  while i<Right do begin
   if CompareFunction(PPointers(DataBase)^[i],PPointers(DataBase)^[i+1])>0 then begin
{$ifdef cpu386}
    asm
     int 3 // hardcoded code breakpoint
    end;
{$endif}
    t:=PPointers(DataBase)^[i];
    PPointers(DataBase)^[i]:=PPointers(DataBase)^[i+1];
    PPointers(DataBase)^[i+1]:=t;
    if i>0 then begin
     dec(i);
    end else begin
     inc(i);
    end;
   end else begin
    inc(i);
   end;
  end;
 end;
{$endif}
begin
 if assigned(DataBase) and (Count>0) and assigned(CompareFunction) then begin
  ProcessSort(0,Count-1,IntLog2(Count)*2);
{$ifdef physicssortdebug}
  ProcessBeRoSort(0,Count-1);
{$endif}
 end;
end;

procedure PhysicsReallocateMemory(var p;Size:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if assigned(pointer(p)) then begin
  if Size=0 then begin
   freemem(pointer(p));
   pointer(p):=nil;
  end else begin
   reallocmem(pointer(p),Size);
  end;
 end else if Size<>0 then begin
  getmem(pointer(p),Size);
 end;
end;

const PhysicsBoxCornerEdges:array[0..11,0..1] of integer=((0,4),
                                                          (0,1),
                                                          (0,3),
                                                          (1,5),
                                                          (1,2),
                                                          (3,7),
                                                          (3,2),
                                                          (4,5),
                                                          (4,7),
                                                          (5,6),
                                                          (7,6),
                                                          (2,6));

function PhysicsBoxGetCorners(const Size:TPhysicsVector3):TPhysicsBoxCorners; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result[0]:=Vector3(-Size.X*0.5,-Size.Y*0.5,-Size.Z*0.5); // 0 ---
 result[1]:=Vector3(Size.X*0.5,-Size.Y*0.5,-Size.Z*0.5);  // 1 +--
 result[2]:=Vector3(Size.X*0.5,-Size.Y*0.5,Size.Z*0.5);   // 2 +-+
 result[3]:=Vector3(-Size.X*0.5,-Size.Y*0.5,Size.Z*0.5);  // 3 --+
 result[4]:=Vector3(-Size.X*0.5,Size.Y*0.5,-Size.Z*0.5);  // 4 -+-
 result[5]:=Vector3(Size.X*0.5,Size.Y*0.5,-Size.Z*0.5);   // 5 ++-
 result[6]:=Vector3(Size.X*0.5,Size.Y*0.5,Size.Z*0.5);    // 6 +++
 result[7]:=Vector3(-Size.X*0.5,Size.Y*0.5,Size.Z*0.5);   // 7 -++
end;

procedure PhysicsObjectInit(var Instance:TPhysicsObject;ABodyType:integer=BodyMesh;ACollisionBodyType:integer=-1); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Axis:integer;
begin
 fillchar(Instance,sizeof(TPhysicsObject),#0);
 with Instance do begin
  BodyType:=ABodyType;
  if ACollisionBodyType<0 then begin
   CollisionBodyType:=ABodyType;
  end else begin
   CollisionBodyType:=ACollisionBodyType;
  end;
  CollisionReceiver:=true;
  CollisionSender:=true;
  HaveNewTransform:=true;
  Transform:=Matrix4x4Identity;
  Matrix4x4Inverse(InvTransform,Transform);
  OldTransform:=Transform;
  OldInvTransform:=InvTransform;
  if assigned(PhysicsInstance) then begin
   ID:=PhysicsInstance^.ObjectID;
   inc(PhysicsInstance^.ObjectID);
   if assigned(PhysicsInstance^.ObjectLast) then begin
    Previous:=PhysicsInstance^.ObjectLast;
    Next:=nil;
    PhysicsInstance^.ObjectLast^.Next:=@Instance;
    PhysicsInstance^.ObjectLast:=@Instance;
   end else begin
    Next:=nil;
    Previous:=nil;
    PhysicsInstance^.ObjectFirst:=@Instance;
    PhysicsInstance^.ObjectLast:=@Instance;
   end;
   for Axis:=0 to 2 do begin
    if assigned(PhysicsInstance^.SweepAndPrune.Lists[Axis].Last) then begin
     SweepAndPruneAllAxis[Axis].Previous:=PhysicsInstance^.SweepAndPrune.Lists[Axis].Last;
     SweepAndPruneAllAxis[Axis].Next:=nil;
     PhysicsInstance^.SweepAndPrune.Lists[Axis].Last^.SweepAndPruneAllAxis[Axis].Next:=@Instance;
     PhysicsInstance^.SweepAndPrune.Lists[Axis].Last:=@Instance;
    end else begin
     SweepAndPruneAllAxis[Axis].Next:=nil;
     SweepAndPruneAllAxis[Axis].Previous:=nil;
     PhysicsInstance^.SweepAndPrune.Lists[Axis].First:=@Instance;
     PhysicsInstance^.SweepAndPrune.Lists[Axis].Last:=@Instance;
    end;
   end;
  end else begin
   ID:=-1;
  end;
 end;
end;

procedure PhysicsObjectDone(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,Axis:integer;
begin
 with Instance do begin
  for i:=0 to NumMeshs-1 do begin
   PhysicsObjectMeshDone(Meshs^[i]^);
   dispose(Meshs^[i]);
   Meshs^[i]:=nil;
  end;
  if assigned(Meshs) then begin
   freemem(Meshs);
   Meshs:=nil;
  end;
  PhysicsObjectMeshDone(BoxMesh);
  PhysicsObjectMeshDone(ConvexHullMesh);
  if assigned(HeightMap.Data) then begin
   freemem(HeightMap.Data);
   HeightMap.Data:=nil;
  end;
  if assigned(SweepAndPruneCache) then begin
   freemem(SweepAndPruneCache);
  end;
  if assigned(SweepAndPrunePairs) then begin
   freemem(SweepAndPrunePairs);
  end;
  if assigned(PhysicsInstance) then begin
   if assigned(PhysicsInstance^.ObjectFirst) then begin
    PhysicsInstance^.ObjectFirst:=Next;
   end;
   if assigned(PhysicsInstance^.ObjectLast) then begin
    PhysicsInstance^.ObjectLast:=Previous;
   end;
   for Axis:=0 to 2 do begin
    if assigned(PhysicsInstance^.SweepAndPrune.Lists[Axis].First) then begin
     PhysicsInstance^.SweepAndPrune.Lists[Axis].First:=SweepAndPruneAllAxis[Axis].Next;
    end;
    if assigned(PhysicsInstance^.SweepAndPrune.Lists[Axis].Last) then begin
     PhysicsInstance^.SweepAndPrune.Lists[Axis].Last:=SweepAndPruneAllAxis[Axis].Previous;
    end;
   end;
  end;
  if assigned(Next) then begin
   Next^.Previous:=Previous;
  end;
  if assigned(Previous) then begin
   Previous^.Next:=Next;
  end;
  for Axis:=0 to 2 do begin
   if assigned(SweepAndPruneAllAxis[Axis].Next) then begin
    SweepAndPruneAllAxis[Axis].Next^.SweepAndPruneAllAxis[Axis].Previous:=SweepAndPruneAllAxis[Axis].Previous;
   end;
   if assigned(SweepAndPruneAllAxis[Axis].Previous) then begin
    SweepAndPruneAllAxis[Axis].Previous^.SweepAndPruneAllAxis[Axis].Next:=SweepAndPruneAllAxis[Axis].Next;
   end;
  end;
 end;
 fillchar(Instance,sizeof(TPhysicsObject),#0);
end;

procedure PhysicsObjectClear(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ABodyType,ACollisionBodyType:integer;
begin
 ABodyType:=Instance.BodyType;
 ACollisionBodyType:=Instance.CollisionBodyType;
 PhysicsObjectDone(Instance);
 PhysicsObjectInit(Instance,ABodyType,ACollisionBodyType);
end;

function PhysicsObjectAddMesh(var Instance:TPhysicsObject):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  result:=NumMeshs;
  inc(NumMeshs);
  PhysicsReallocateMemory(Meshs,NumMeshs*sizeof(PPhysicsObjectMesh));
  new(Meshs^[result]);
  fillchar(Meshs^[result]^,sizeof(TPhysicsObjectMesh),#0);
 end;
end;

function PhysicsObjectMeshAddMesh(var Instance:TPhysicsObjectMesh):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  result:=NumMeshs;
  inc(NumMeshs);
  PhysicsReallocateMemory(Meshs,NumMeshs*sizeof(PPhysicsObjectMesh));
  new(Meshs^[result]);
  fillchar(Meshs^[result]^,sizeof(TPhysicsObjectMesh),#0);
 end;
end;

procedure PhysicsObjectMeshDone(var Instance:TPhysicsObjectMesh); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
begin
 with Instance do begin
  for i:=0 to NumMeshs-1 do begin
   PhysicsObjectMeshDone(Meshs^[i]^);
   dispose(Meshs^[i]);
   Meshs^[i]:=nil;
  end;
  if assigned(Meshs) then begin
   freemem(Meshs);
   Meshs:=nil;
  end;
  if assigned(Triangles) then begin
   freemem(Triangles);
   Triangles:=nil;
  end;
 end;
 fillchar(Instance,sizeof(TPhysicsObjectMesh),#0);
end;

procedure PhysicsObjectMeshAddTriangle(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsTriangleVertices); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var t:TPhysicsTriangle;
begin
 with Instance do begin
  fillchar(t,sizeof(TPhysicsTriangle),#0);
  t.Vertices:=Vectors^;
  inc(NumTriangles);
  if NumTriangles>=AllTriangles then begin
   AllTriangles:=(NumTriangles+MemoryInc) and not MemoryIncMask;
   PhysicsReallocateMemory(Triangles,AllTriangles*sizeof(TPhysicsTriangle));
  end;
  Triangles^[NumTriangles-1]:=t;
 end;
end;

procedure PhysicsObjectMeshAddTriangle(var Instance:TPhysicsObjectMesh;const v0,v1,v2:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var t:TPhysicsTriangle;
begin
 with Instance do begin
  fillchar(t,sizeof(TPhysicsTriangle),#0);
  t.Vertices[0]:=v0;
  t.Vertices[1]:=v1;
  t.Vertices[2]:=v2;
  inc(NumTriangles);
  if NumTriangles>=AllTriangles then begin
   AllTriangles:=(NumTriangles+MemoryInc) and not MemoryIncMask;
   PhysicsReallocateMemory(Triangles,AllTriangles*sizeof(TPhysicsTriangle));
  end;
  Triangles^[NumTriangles-1]:=t;
 end;
end;

procedure PhysicsObjectMeshAddTriangles(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsTriangleVertices;Count:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
begin
 for i:=1 to Count do begin
  PhysicsObjectMeshAddTriangle(Instance,Vectors);
  inc(Vectors);
 end;
end;

procedure PhysicsObjectMeshAddQuad(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsQuadVertices); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 PhysicsObjectMeshAddTriangle(Instance,Vectors^[0],Vectors^[1],Vectors^[2]);
 PhysicsObjectMeshAddTriangle(Instance,Vectors^[2],Vectors^[3],Vectors^[0]);
end;

procedure PhysicsObjectMeshAddQuad(var Instance:TPhysicsObjectMesh;const v0,v1,v2,v3:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 PhysicsObjectMeshAddTriangle(Instance,v0,v1,v2);
 PhysicsObjectMeshAddTriangle(Instance,v2,v3,v0);
end;

procedure PhysicsObjectMeshAddQuads(var Instance:TPhysicsObjectMesh;Vectors:PPhysicsQuadVertices;Count:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
begin
 for i:=1 to Count do begin
  PhysicsObjectMeshAddQuad(Instance,Vectors);
  inc(Vectors);
 end;
end;

procedure PhysicsObjectMeshCreateBox(var Instance:TPhysicsObjectMesh;SizeX,SizeY,SizeZ:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var v:array[0..7] of TPhysicsVector3;
begin
 v[0]:=Vector3(-SizeX*0.5,-SizeY*0.5,-SizeZ*0.5);
 v[1]:=Vector3(SizeX*0.5,-SizeY*0.5,-SizeZ*0.5);
 v[2]:=Vector3(SizeX*0.5,-SizeY*0.5,SizeZ*0.5);
 v[3]:=Vector3(-SizeX*0.5,-SizeY*0.5,SizeZ*0.5);
 v[4]:=Vector3(-SizeX*0.5,SizeY*0.5,-SizeZ*0.5);
 v[5]:=Vector3(SizeX*0.5,SizeY*0.5,-SizeZ*0.5);
 v[6]:=Vector3(SizeX*0.5,SizeY*0.5,SizeZ*0.5);
 v[7]:=Vector3(-SizeX*0.5,SizeY*0.5,SizeZ*0.5);
 Instance.NumTriangles:=0;
 PhysicsObjectMeshAddQuad(Instance,v[4],v[5],v[1],v[0]);
 PhysicsObjectMeshAddQuad(Instance,v[5],v[6],v[2],v[1]);
 PhysicsObjectMeshAddQuad(Instance,v[6],v[7],v[3],v[2]);
 PhysicsObjectMeshAddQuad(Instance,v[7],v[4],v[0],v[3]);
 PhysicsObjectMeshAddQuad(Instance,v[0],v[1],v[2],v[3]);
 PhysicsObjectMeshAddQuad(Instance,v[7],v[6],v[5],v[4]);
end;

procedure PhysicsObjectMeshCreateSphere(var Instance:TPhysicsObjectMesh;r:TPhysicsFloat;n:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j,k:integer;
    Theta1,Theta2,Theta3,Theta4,ex,ey,ez:TPhysicsFloat;
    v:TPhysicsQuadVertices;
begin
 Instance.NumTriangles:=0;
 r:=abs(r);
 n:=abs(n);
 if (n<4) or (r<=0) then begin
  exit;
 end;
 k:=(n div 2)-1;
 for j:=0 to k do begin
  Theta1:=j*2*pi/n-(pi*0.5);
  Theta2:=(j+1)*2*pi/n-(pi*0.5);
  for i:=0 to n-1 do begin
   Theta3:=i*2*pi/n;
   Theta4:=(i+1)*2*pi/n;

   ex:=cos(Theta1)*cos(Theta3);
   ey:=sin(Theta1);
   ez:=cos(Theta1)*sin(Theta3);
   v[0]:=Vector3(r*ex,r*ey,r*ez);

   ex:=cos(Theta2)*cos(Theta3);
   ey:=sin(Theta2);
   ez:=cos(Theta2)*sin(Theta3);
   v[1]:=Vector3(r*ex,r*ey,r*ez);

   ex:=cos(Theta2)*cos(Theta4);
   ey:=sin(Theta2);
   ez:=cos(Theta2)*sin(Theta4);
   v[2]:=Vector3(r*ex,r*ey,r*ez);

   ex:=cos(Theta1)*cos(Theta4);
   ey:=sin(Theta1);
   ez:=cos(Theta1)*sin(Theta4);
   v[3]:=Vector3(r*ex,r*ey,r*ez);

   PhysicsObjectMeshAddQuad(Instance,@v);
  end;
 end;
end;

procedure PhysicsObjectMeshCreateCylinder(var Instance:TPhysicsObjectMesh;r,l:TPhysicsFloat;n:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    Theta1,Theta2:TPhysicsFloat;
    v:TPhysicsQuadVertices;
begin
 Instance.NumTriangles:=0;
 r:=abs(r);
 n:=abs(n);
 if (n<4) or (r<=0) then begin
  exit;
 end;
 for i:=0 to n do begin
  Theta1:=i*2*pi/n-(pi*0.5);
  Theta2:=(i+1)*2*pi/n-(pi*0.5);
  v[0]:=Vector3(cos(Theta1)*r,sin(Theta1)*r,-l*0.5);
  v[1]:=Vector3(cos(Theta2)*r,sin(Theta2)*r,-l*0.5);
  v[2]:=Vector3(cos(Theta2)*r,sin(Theta2)*r,l*0.5);
  v[3]:=Vector3(cos(Theta1)*r,sin(Theta1)*r,l*0.5);
  PhysicsObjectMeshAddQuad(Instance,@v);
 end;
 for i:=0 to n do begin
  Theta1:=i*2*pi/n-(pi*0.5);
  Theta2:=(i+1)*2*pi/n-(pi*0.5);
  v[0]:=Vector3(0,0,l*0.5);
  v[1]:=Vector3(0,0,l*0.5);
  v[2]:=Vector3(cos(Theta1)*r,sin(Theta1)*r,l*0.5);
  v[3]:=Vector3(cos(Theta2)*r,sin(Theta2)*r,l*0.5);
  PhysicsObjectMeshAddQuad(Instance,@v);
 end;
 for i:=0 to n do begin
  Theta1:=i*2*pi/n-(pi*0.5);
  Theta2:=(i+1)*2*pi/n-(pi*0.5);
  v[0]:=Vector3(cos(Theta2)*r,sin(Theta2)*r,-l*0.5);
  v[1]:=Vector3(cos(Theta1)*r,sin(Theta1)*r,-l*0.5);
  v[2]:=Vector3(0,0,-l*0.5);
  v[3]:=Vector3(0,0,-l*0.5);
  PhysicsObjectMeshAddQuad(Instance,@v);
 end;
end;

procedure PhysicsObjectMeshCreateCapsule(var Instance:TPhysicsObjectMesh;r,l:TPhysicsFloat;n:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j,k:integer;
    Theta1,Theta2,Theta3,Theta4,ex,ey,ez,nh,rh:TPhysicsFloat;
    v:TPhysicsQuadVertices;
begin
 Instance.NumTriangles:=0;
 r:=abs(r);
 n:=abs(n);
 if (n<4) or (r<=0) then begin
  exit;
 end;
 k:=(n div 2)-1;
 nh:=l*0.5;
 for i:=0 to n do begin
  Theta1:=i*2*pi/n-(pi*0.5);
  Theta2:=(i+1)*2*pi/n-(pi*0.5);
  v[0]:=Vector3(cos(Theta1)*r,sin(Theta1)*r,-nh);
  v[1]:=Vector3(cos(Theta2)*r,sin(Theta2)*r,-nh);
  v[2]:=Vector3(cos(Theta2)*r,sin(Theta2)*r,nh);
  v[3]:=Vector3(cos(Theta1)*r,sin(Theta1)*r,nh);
  PhysicsObjectMeshAddQuad(Instance,@v);                       
 end;
 rh:=r;
 for j:=0 to k div 2 do begin
  Theta1:=j*2*pi/n-(pi*0.5);
  Theta2:=(j+1)*2*pi/n-(pi*0.5);
  for i:=0 to n do begin
   Theta3:=i*2*pi/n;
   Theta4:=(i+1)*2*pi/n;

   ex:=cos(Theta2)*cos(Theta3);
   ey:=sin(Theta2);
   ez:=cos(Theta2)*sin(Theta3);
   v[3]:=Vector3(r*ex,r*ez,nh-(ey*rh));

   ex:=cos(Theta1)*cos(Theta3);
   ey:=sin(Theta1);
   ez:=cos(Theta1)*sin(Theta3);
   v[2]:=Vector3(r*ex,r*ez,nh-(ey*rh));

   ex:=cos(Theta1)*cos(Theta4);
   ey:=sin(Theta1);
   ez:=cos(Theta1)*sin(Theta4);
   v[1]:=Vector3(r*ex,r*ez,nh-(ey*rh));

   ex:=cos(Theta2)*cos(Theta4);
   ey:=sin(Theta2);
   ez:=cos(Theta2)*sin(Theta4);
   v[0]:=Vector3(r*ex,r*ez,nh-(ey*rh));

   PhysicsObjectMeshAddQuad(Instance,@v);
  end;
 end;
 for j:=0 to k div 2 do begin
  Theta1:=j*2*pi/n-(pi*0.5);
  Theta2:=(j+1)*2*pi/n-(pi*0.5);
  for i:=0 to n do begin
   Theta3:=i*2*pi/n;
   Theta4:=(i+1)*2*pi/n;

   ex:=cos(Theta2)*cos(Theta3);
   ey:=sin(Theta2);
   ez:=cos(Theta2)*sin(Theta3);
   v[0]:=Vector3(r*ex,r*ez,-nh+(ey*rh));

   ex:=cos(Theta1)*cos(Theta3);
   ey:=sin(Theta1);
   ez:=cos(Theta1)*sin(Theta3);
   v[1]:=Vector3(r*ex,r*ez,-nh+(ey*rh));

   ex:=cos(Theta1)*cos(Theta4);
   ey:=sin(Theta1);
   ez:=cos(Theta1)*sin(Theta4);
   v[2]:=Vector3(r*ex,r*ez,-nh+(ey*rh));

   ex:=cos(Theta2)*cos(Theta4);
   ey:=sin(Theta2);
   ez:=cos(Theta2)*sin(Theta4);
   v[3]:=Vector3(r*ex,r*ez,-nh+(ey*rh));

   PhysicsObjectMeshAddQuad(Instance,@v);
  end;
 end;
end;

procedure PhysicsObjectMeshCreatePlane(var Instance:TPhysicsObjectMesh;Plane:TPhysicsPlane); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var AABB:array[0..5] of TPhysicsFloat;
begin
 Instance.MeshPlane:=Plane;
 AABB[0]:=-INFINITY;
 AABB[1]:=INFINITY;
 AABB[2]:=-INFINITY;
 AABB[3]:=INFINITY;
 AABB[4]:=-INFINITY;
 AABB[5]:=INFINITY;
 if (abs(Plane.b)<EPSILON) and (abs(Plane.c)<EPSILON) then begin
  if Plane.a>0 then begin
   AABB[1]:=Plane.d;
  end else begin
   AABB[0]:=-Plane.d;
  end;
 end else if (abs(Plane.a)<EPSILON) and (abs(Plane.c)<EPSILON) then begin
  if Plane.b>0 then begin
   AABB[3]:=Plane.d;
  end else begin
   AABB[2]:=-Plane.d;
  end;
 end else if (abs(Plane.a)<EPSILON) and (abs(Plane.b)<EPSILON) then begin
  if Plane.c>0 then begin
   AABB[5]:=Plane.d;
  end else begin
   AABB[4]:=-Plane.d;
  end;
 end;
 PhysicsObjectMeshCreateBox(Instance,AABB[1]-AABB[0],AABB[3]-AABB[2],AABB[5]-AABB[4]);
 Instance.AABB.Min:=Vector3(AABB[0],AABB[2],AABB[4]);
 Instance.AABB.Max:=Vector3(AABB[1],AABB[3],AABB[5]);
 Instance.Sphere.Center:=Vector3ScalarMul(Vector3Add(Instance.AABB.Min,Instance.AABB.Max),0.5);
 Instance.Sphere.Radius:=Max(AABB[1]-AABB[0],Max(AABB[3]-AABB[2],AABB[5]-AABB[4]))*0.5;
end;

function PhysicsObjectMeshSubdivideFindBestAxis(var Mesh:TPhysicsObjectMesh;var Left,Right:integer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j,AxisResult:integer;
    Center:TPhysicsVector3;
    AxisResults,LeftCount,RightCount:array[0..2] of integer;
    c:TPhysicsFloat;
begin
 Center:=Vector3ScalarMul(Vector3Add(Mesh.AABB.Min,Mesh.AABB.Max),0.5);

 for i:=0 to 2 do begin
  LeftCount[i]:=0;
  RightCount[i]:=0;
  for j:=0 to Mesh.NumTriangles-1 do begin
   c:=(Mesh.Triangles^[j].Vertices[0].xyz[i]+Mesh.Triangles^[j].Vertices[1].xyz[i]+Mesh.Triangles^[j].Vertices[2].xyz[i])*fCI3;
   if c<=Center.xyz[i] then begin
    inc(LeftCount[i]);
   end else begin
    inc(RightCount[i]);
   end;
  end;
  AxisResults[i]:=abs(LeftCount[i]-RightCount[i]);
 end;

 result:=0;
 AxisResult:=AxisResults[0];
 Left:=LeftCount[0];
 Right:=LeftCount[0];
 for i:=1 to 2 do begin
  if AxisResults[i]<AxisResult then begin
   AxisResult:=AxisResults[i];
   Left:=LeftCount[i];
   Right:=LeftCount[i];
   result:=i;
  end;
 end;
end;

procedure PhysicsObjectMeshSubdivide(var Mesh:TPhysicsObjectMesh;TrianglesMinThreshold:integer=4;MaxDepth:integer=64;Level:integer=0); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j,k,BestAxis,Left,Right:integer;
    Center,v:TPhysicsVector3;
    DstMesh:PPhysicsObjectMesh;
    r:TPhysicsFloat;
begin
 if (Mesh.NumTriangles<TrianglesMinThreshold) or (Mesh.NumMeshs>0) or (Level>=MaxDepth) then exit;

 for i:=0 to Mesh.NumTriangles-1 do begin
  for j:=0 to 2 do begin
   v:=Mesh.Triangles^[i].Vertices[j];
   if (i=0) and (j=0) then begin
    Mesh.AABB.Min:=v;
    Mesh.AABB.Max:=v;
   end else begin
    if Mesh.AABB.Min.x>v.x then Mesh.AABB.Min.x:=v.x;
    if Mesh.AABB.Min.y>v.y then Mesh.AABB.Min.y:=v.y;
    if Mesh.AABB.Min.z>v.z then Mesh.AABB.Min.z:=v.z;
    if Mesh.AABB.Max.x<v.x then Mesh.AABB.Max.x:=v.x;
    if Mesh.AABB.Max.y<v.y then Mesh.AABB.Max.y:=v.y;
    if Mesh.AABB.Max.z<v.z then Mesh.AABB.Max.z:=v.z;
   end;
  end;
 end;

 Mesh.Sphere.Center:=Vector3ScalarMul(Vector3Add(Mesh.AABB.Min,Mesh.AABB.Max),0.5);
 Mesh.Sphere.Radius:=0;
 for j:=0 to Mesh.NumTriangles-1 do begin
  for k:=0 to 2 do begin
   v:=Mesh.Triangles^[j].Vertices[k];
   r:=Vector3Length(Vector3Sub(Mesh.Sphere.Center,v));
   if Mesh.Sphere.Radius<r then begin
    Mesh.Sphere.Radius:=r;
   end;
  end;
 end;

 BestAxis:=PhysicsObjectMeshSubdivideFindBestAxis(Mesh,Left,Right);

 PhysicsObjectMeshAddMesh(Mesh);
 PhysicsObjectMeshAddMesh(Mesh);

 if (Left<>0) and (Right<>0) then begin
  Center:=Vector3ScalarMul(Vector3Add(Mesh.AABB.Min,Mesh.AABB.Max),0.5);

  for i:=0 to 1 do begin
   DstMesh:=Mesh.Meshs^[i];
   DstMesh^.NumTriangles:=0;
  end;

  for i:=0 to Mesh.NumTriangles-1 do begin
   if ((Mesh.Triangles^[i].Vertices[0].xyz[BestAxis]+Mesh.Triangles^[i].Vertices[1].xyz[BestAxis]+Mesh.Triangles^[i].Vertices[2].xyz[BestAxis])*fCI3)<=Center.xyz[BestAxis] then begin
    DstMesh:=Mesh.Meshs^[0];
   end else begin
    DstMesh:=Mesh.Meshs^[1];
   end;
   inc(DstMesh^.NumTriangles);
  end;

  for i:=0 to 1 do begin
   DstMesh:=Mesh.Meshs^[i];
   getmem(DstMesh^.Triangles,DstMesh^.NumTriangles*sizeof(TPhysicsTriangle));
   DstMesh^.NumTriangles:=0;
  end;

  for i:=0 to Mesh.NumTriangles-1 do begin
   if ((Mesh.Triangles^[i].Vertices[0].xyz[BestAxis]+Mesh.Triangles^[i].Vertices[1].xyz[BestAxis]+Mesh.Triangles^[i].Vertices[2].xyz[BestAxis])*fCI3)<=Center.xyz[BestAxis] then begin
    DstMesh:=Mesh.Meshs^[0];
   end else begin
    DstMesh:=Mesh.Meshs^[1];
   end;
   DstMesh^.Triangles^[DstMesh^.NumTriangles]:=Mesh.Triangles^[i];
   inc(DstMesh^.NumTriangles);
  end;
 end else begin
  j:=Mesh.NumTriangles shr 1;

  for i:=0 to 1 do begin
   DstMesh:=Mesh.Meshs^[i];
   DstMesh^.NumTriangles:=0;
  end;

  for i:=0 to Mesh.NumTriangles-1 do begin
   if i<j then begin
    DstMesh:=Mesh.Meshs^[0];
   end else begin
    DstMesh:=Mesh.Meshs^[1];
   end;
   inc(DstMesh^.NumTriangles);
  end;

  for i:=0 to 1 do begin
   DstMesh:=Mesh.Meshs^[i];
   getmem(DstMesh^.Triangles,DstMesh^.NumTriangles*sizeof(TPhysicsTriangle));
   DstMesh^.NumTriangles:=0;
  end;

  for i:=0 to Mesh.NumTriangles-1 do begin
   if i<j then begin
    DstMesh:=Mesh.Meshs^[0];
   end else begin
    DstMesh:=Mesh.Meshs^[1];
   end;
   DstMesh^.Triangles^[DstMesh^.NumTriangles]:=Mesh.Triangles^[i];
   inc(DstMesh^.NumTriangles);
  end;
 end;

 Mesh.NumTriangles:=0;
 if assigned(Mesh.Triangles) then begin
  freemem(Mesh.Triangles);
  Mesh.Triangles:=nil;
 end;

 PhysicsObjectMeshSubdivide(Mesh.Meshs^[0]^,TrianglesMinThreshold,MaxDepth,Level+1);
 PhysicsObjectMeshSubdivide(Mesh.Meshs^[1]^,TrianglesMinThreshold,MaxDepth,Level+1);
end;

procedure PhysicsObjectMeshFinish(var Instance:TPhysicsObjectMesh); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    n:TPhysicsVector3;
 procedure AABBCheckMesh(var AABB:TPhysicsAABB;var Mesh:TPhysicsObjectMesh;First:boolean);
 var i,j:integer;
     v:TPhysicsVector3;
 begin
  for i:=0 to Mesh.NumTriangles-1 do begin
   for j:=0 to 2 do begin
    v:=Mesh.Triangles^[i].Vertices[j];
    if First then begin
     First:=false;
     AABB.Min:=v;
     AABB.Max:=v;
    end else begin
     if AABB.Min.x>v.x then AABB.Min.x:=v.x;
     if AABB.Min.y>v.y then AABB.Min.y:=v.y;
     if AABB.Min.z>v.z then AABB.Min.z:=v.z;
     if AABB.Max.x<v.x then AABB.Max.x:=v.x;
     if AABB.Max.y<v.y then AABB.Max.y:=v.y;
     if AABB.Max.z<v.z then AABB.Max.z:=v.z;
    end;
   end;
  end;
  for i:=0 to Mesh.NumMeshs-1 do begin
   AABBCheckMesh(AABB,Mesh.Meshs^[i]^,First);
  end;
 end;
 procedure SphereCheckMesh(var Sphere:TPhysicsSphere;var Mesh:TPhysicsObjectMesh);
 var i,j:integer;
     v:TPhysicsVector3;
     r:TPhysicsFloat;
 begin
  for i:=0 to Mesh.NumTriangles-1 do begin
   for j:=0 to 2 do begin
    v:=Mesh.Triangles^[i].Vertices[j];
    r:=Vector3Length(Vector3Sub(Sphere.Center,v));
    if Sphere.Radius<r then begin
     Sphere.Radius:=r;
    end;
   end;
  end;
  for i:=0 to Mesh.NumMeshs-1 do begin
   SphereCheckMesh(Sphere,Mesh.Meshs^[i]^);
  end;
 end;
begin
 with Instance do begin
  for i:=0 to NumMeshs-1 do begin
   PhysicsObjectMeshFinish(Meshs^[i]^);
  end;
  AABB.Min:=Vector3Origin;
  AABB.Max:=Vector3Origin;
  AABBCheckMesh(AABB,Instance,true);
  Sphere.Center:=Vector3ScalarMul(Vector3Add(AABB.Min,AABB.Max),0.5);
  Sphere.Radius:=0;
  SphereCheckMesh(Sphere,Instance);
  for i:=0 to NumTriangles-1 do begin
   n:=Vector3Norm(Vector3Cross(Vector3Sub(Triangles^[i].Vertices[1],Triangles^[i].Vertices[0]),Vector3Sub(Triangles^[i].Vertices[2],Triangles^[i].Vertices[0])));
   Triangles^[i].Plane:=Plane(n,-Vector3Dot(n,Triangles^[i].Vertices[0]));

   n:=Vector3Norm(Vector3Cross(Triangles^[i].Plane.Normal,Vector3Sub(Triangles^[i].Vertices[0],Triangles^[i].Vertices[2])));
   Triangles^[i].FastTriangleCheckPlanes[0]:=Plane(n,-Vector3Dot(n,Triangles^[i].Vertices[0]));

   n:=Vector3Norm(Vector3Cross(Triangles^[i].Plane.Normal,Vector3Sub(Triangles^[i].Vertices[1],Triangles^[i].Vertices[0])));
   Triangles^[i].FastTriangleCheckPlanes[1]:=Plane(n,-Vector3Dot(n,Triangles^[i].Vertices[1]));

   n:=Vector3Norm(Vector3Cross(Triangles^[i].Plane.Normal,Vector3Sub(Triangles^[i].Vertices[2],Triangles^[i].Vertices[1])));
   Triangles^[i].FastTriangleCheckPlanes[2]:=Plane(n,-Vector3Dot(n,Triangles^[i].Vertices[2]));
  end;
 end;
end;

procedure PhysicsObjectMeshTranslate(var Instance:TPhysicsObjectMesh;Translation:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
 procedure CheckMesh(var Mesh:TPhysicsObjectMesh);
 var i,j:integer;
 begin
  for i:=0 to Mesh.NumTriangles-1 do begin
   for j:=0 to 2 do begin
    Mesh.Triangles^[i].Vertices[j]:=Vector3Add(Mesh.Triangles^[i].Vertices[j],Translation);
   end;
  end;
 end;
var i:integer;
begin
 with Instance do begin
  for i:=0 to NumMeshs-1 do begin
   CheckMesh(Meshs^[i]^);
  end;
 end;
end;

procedure PhysicsObjectTranslate(var Instance:TPhysicsObject;Translation:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
begin
 with Instance do begin
  for i:=0 to NumMeshs-1 do begin
   PhysicsObjectMeshTranslate(Meshs^[i]^,Translation);
  end;
 end;
end;

procedure PhysicsObjectMeshOptimizeToLevelOfDetail(var Mesh:TPhysicsObjectMesh;LOD:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const INFINITY=(34e+34)-1;
type TFloat={$ifdef cpu386}extended{$else}double{$endif};
     TIntegers=array[0..0] of integer;
     PIntegers=^TIntegers;
     TVector3=packed record
      x,y,z:TFloat;
     end;
     TVertex=TVector3;
     TVertices=array[0..3330] of TVertex;
     PVertices=^TVertices;
     TTriangle=record
      a,b,c:integer;
     end;
     TTriangles=array[0..0] of TTriangle;
     PTriangles=^TTriangles;
     TNeighbor=record
      index:integer;
      count:integer;
     end;
     PNeighbors=^TNeighbors;
     TNeighbors=array[0..0] of TNeighbor;
     TVertexInfo=record
      Neighbors:PNeighbors;
      NumNeighbors:integer;
      Faces:PIntegers;
      NumFaces:integer;
      Collapse:integer;
      Distance:TPhysicsFloat;
     end;
     PVertexInfos=^TVertexInfos;
     TVertexInfos=array[0..0] of TVertexInfo;
     TFaceInfo=record
      x,y,z:TFloat;
      collapsed:boolean;
     end;
     TFaceInfos=array[0..0] of TFaceInfo;
     PFaceInfos=^TFaceInfos;
var Vertices:PVertices;
    NumVertices:integer;
    Triangles:PTriangles;
    NumTriangles:integer;
    VertexInfos:PVertexInfos;
    NumVertexInfos:integer;
    FaceInfos:PFaceInfos;
    NumFaceInfos:integer;
 function Vector3Compare(const v1,v2:TVector3):boolean;
 begin
  result:=(abs(v1.x-v2.x)<EPSILON) and (abs(v1.y-v2.y)<EPSILON) and (abs(v1.z-v2.z)<EPSILON);
 end;
 function Vector3Sub(const v1,v2:TVector3):TVector3;
 begin
  result.x:=v1.x-v2.x;
  result.y:=v1.y-v2.y;
  result.z:=v1.z-v2.z;
 end;
 function Vector3Add(const v1,v2:TVector3):TVector3;
 begin
  result.x:=v1.x+v2.x;
  result.y:=v1.y+v2.y;
  result.z:=v1.z+v2.z;
 end;
 function Vector3Cross(const v1,v2:TVector3):TVector3;
 begin
  result.x:=(v1.y*v2.z)-(v1.z*v2.y);
  result.y:=(v1.z*v2.x)-(v1.x*v2.z);
  result.z:=(v1.x*v2.y)-(v1.y*v2.x);
 end;
 function Vector3Length(const v:TVector3):TFloat;
 begin
  result:=sqrt(sqr(v.x)+sqr(v.y)+sqr(v.z));
 end;
 function Vector3LengthSquared(const v:TVector3):TFloat;
 begin
  result:=sqr(v.x)+sqr(v.y)+sqr(v.z);
 end;
 function Vector3ScalarMul(const v:TVector3;s:TFloat):TVector3;
 begin
  result.x:=v.x*s;
  result.y:=v.y*s;
  result.z:=v.z*s;
 end;
 function Vector3Dot(const v1,v2:TVector3):TFloat;
 begin
  result:=(v1.x*v2.x)+(v1.y*v2.y)+(v1.z*v2.z);
 end;
 function Vector3Norm(const v:TVector3):TVector3;
 var l:TFloat;
 begin
  l:=Vector3Length(v);
  if abs(l)<EPSILON then begin
   result.x:=0;
   result.y:=0;
   result.z:=0;
  end else begin
   result:=Vector3ScalarMul(v,1/l);
  end;
 end;
 function Vector3Dist(const v1,v2:TVector3):TFloat;
 begin
  result:=Vector3Length(Vector3Sub(v2,v1));
 end;
 function Vector3(v:TPhysicsVector3):TVector3;
 begin
  result.x:=v.x;
  result.y:=v.y;
  result.z:=v.z;
 end;
 function Vector3Ex(v:TVector3):TPhysicsVector3;
 begin
  result.x:=v.x;
  result.y:=v.y;
  result.z:=v.z;
 end;
 procedure AddNeighbor(vertex,Neighbor:integer);
 var i:integer;
 begin
  if vertex=Neighbor then exit;
  with VertexInfos^[vertex] do begin
   for i:=0 to NumNeighbors-1 do begin
    if Neighbors^[i].Index=Neighbor then begin
     inc(Neighbors^[i].Count);
     exit;
    end;
   end;
   i:=NumNeighbors;
   inc(NumNeighbors);
   PhysicsReallocateMemory(Neighbors,NumNeighbors*sizeof(TNeighbor));
   fillchar(Neighbors^[i],sizeof(TNeighbor),#0);
   Neighbors^[i].Index:=Neighbor;
  end;
 end;
 procedure DelNeighbor(vertex,Neighbor:integer);
 var l,i:integer;
 begin
  if vertex=Neighbor then exit;
  with VertexInfos^[vertex] do begin
   l:=NumNeighbors-1;
   for i:=0 to l do begin
    if Neighbors^[i].Index=Neighbor then begin
     if Neighbors^[i].Count>0 then begin
      dec(Neighbors^[i].Count);
     end else begin
      Neighbors^[i]:=Neighbors^[l];
      NumNeighbors:=l;
      PhysicsReallocateMemory(Neighbors,NumNeighbors*sizeof(TNeighbor));
     end;
     exit;
    end;
   end;
  end;
 end;
 procedure NewNeighbor(vertex,oldneighbor,newneighbor:integer);
 begin
  if (vertex=oldneighbor) or (vertex=newneighbor) or (oldneighbor=newneighbor) then begin
   exit;
  end;
  DelNeighbor(vertex,oldNeighbor);
  AddNeighbor(vertex,NewNeighbor);
 end;
 procedure AddTriangle(vertex,triangle:integer);
 var i:integer;
 begin
  with VertexInfos^[vertex] do begin
   i:=NumFaces;
   inc(NumFaces);
   PhysicsReallocateMemory(Faces,NumFaces*sizeof(integer));
   Faces^[i]:=triangle;
   with Triangles^[triangle] do begin
    AddNeighbor(vertex,a);
    AddNeighbor(vertex,b);
    AddNeighbor(vertex,c);
   end;
  end;
 end;
 procedure DelTriangle(vertex,triangle:integer);
 var i,l:integer;
 begin
  with VertexInfos^[vertex] do begin
   if Distance>INFINITY then exit;
   l:=NumFaces-1;
   i:=l;
   while i>=0 do begin
    if Faces^[i]=triangle then begin
     Faces^[i]:=Faces^[l];
     NumFaces:=l;
     PhysicsReallocateMemory(Faces,NumFaces*sizeof(integer));
     with Triangles^[triangle] do begin
      DelNeighbor(vertex,a);
      DelNeighbor(vertex,b);
      DelNeighbor(vertex,c);
     end;
     exit;
    end;
    dec(i);
   end;
  end;
 end;
 function dotproduct(a,b:integer):TFloat;
 begin
  result:=(FaceInfos^[a].x*FaceInfos^[b].x)+(FaceInfos^[a].y*FaceInfos^[b].y)+(FaceInfos^[a].z*FaceInfos^[b].z);
 end;
 function CollapseCost(u,v:integer):TFloat;
 var dx,dy,dz,len,curvature,mincurv,curv:TFloat;
     sides:pintegers;
     scount,i,j,f:integer;
 begin
  dx:=Vertices[u].x-Vertices[v].x;
  dy:=Vertices[u].y-Vertices[v].y;
  dz:=Vertices[u].z-Vertices[v].z;
  len:=sqrt(sqr(dx)+sqr(dy)+sqr(dz));
  curvature:=0;
  scount:=0;
  with VertexInfos^[u] do begin
   getmem(sides,NumFaces*sizeof(integer));
   fillchar(sides^,NumFaces*sizeof(integer),#0);
   for i:=0 to NumFaces-1 do begin
    f:=Faces^[i];
    with Triangles^[f] do begin
     if (a=v) or (b=v) or (c=v) then begin
      sides^[scount]:=f;
      inc(scount);
     end;
    end;
   end;
   for i:=0 to NumFaces-1 do begin
    mincurv:=1;
    for j:=0 to scount-1 do begin
     curv:=(1-dotproduct(faces[i],sides[j]))*0.5;
     if curv<mincurv then mincurv:=curv;
    end;
    if mincurv>curvature then curvature:=mincurv;
   end;
  end;
  freemem(sides);
  result:=len*curvature;
 end;
 procedure ComputeNormal(Index:integer);
 var x1,y1,z1,x2,y2,z2,nx,ny,nz,vl:TPhysicsFloat;
 begin
  with Triangles^[Index] do begin
   x1:=Vertices^[b].x-Vertices^[a].x;
   y1:=Vertices^[b].y-Vertices^[a].y;
   z1:=Vertices^[b].z-Vertices^[a].z;
   x2:=Vertices^[c].x-Vertices^[a].x;
   y2:=Vertices^[c].y-Vertices^[a].y;
   z2:=Vertices^[c].z-Vertices^[a].z;
   nx:=(y1*z2)-(y2*z1);
   ny:=(z1*x2)-(z2*x1);
   nz:=(x1*y2)-(x2*y1);
   vl:=sqrt(sqr(nx)+sqr(ny)+sqr(nz));
   if abs(vl)>EPSILON then begin
    vl:=1/vl;
    nx:=nx*vl;
    ny:=ny*vl;
    nz:=nz*vl;
   end;
  end;
  with FaceInfos^[Index] do begin
   x:=nx;
   y:=ny;
   z:=nz;
  end;
 end;
 procedure ComputeVertexCost(Index:integer);
 var i,j:integer;
     l:TPhysicsFloat;
 begin
  with VertexInfos^[Index] do begin
   if Distance>INFINITY then exit;
   if NumNeighbors=0 then begin
    Collapse:=-1;
    Distance:=0;
   end else begin
    Collapse:=Neighbors^[0].index;
    Distance:=CollapseCost(Index,Collapse);
    for i:=1 to NumNeighbors-1 do begin
     j:=Neighbors^[i].index;
     l:=CollapseCost(Index,j);
     if l<Distance then begin
      Collapse:=j;
      Distance:=l;
     end;
    end;
   end;
  end;
 end;
 function CollapseVertex:integer;
 var i,j,f:integer;
     d,d2:TPhysicsFloat;
 begin
  result:=0;
  i:=0;
  d:=VertexInfos^[i].Distance;
  for j:=1 to NumVertices-1 do begin
   d2:=VertexInfos^[j].Distance;
   if d2<d then begin
    d:=d2;
    i:=j;
   end;
  end;
  if d>INFINITY then exit;
  with VertexInfos^[i] do begin
   Distance:=INFINITY+1;
   j:=0;
   while j<NumFaces do begin
    f:=Faces^[j];
    with Triangles^[f] do begin
     if (a=Collapse) or (b=Collapse) or (c=Collapse) then begin
      if FaceInfos^[f].Collapsed then exit;
      FaceInfos^[f].Collapsed:=true;
      DelTriangle(a,f);
      DelTriangle(b,f);
      DelTriangle(c,f);
      inc(result);
     end else begin
      if a=i then begin
       a:=Collapse;
      end else if b=i then begin
       b:=Collapse;
      end else if c=i then begin
       c:=Collapse;
      end else begin
       exit;
      end;
      AddTriangle(collapse,f);
      NewNeighbor(a,i,collapse);
      NewNeighbor(b,i,collapse);
      NewNeighbor(c,i,collapse);
      ComputeNormal(f);
     end;
    end;
    inc(j);
   end;
   for j:=0 to NumNeighbors-1 do begin
    ComputeVertexCost(Neighbors^[j].Index);
   end;
  end;
 end;
 procedure FreezeBorders;
 var i,j,k,point,count:integer;
 begin
  for i:=0 to NumVertexInfos-1 do begin
   with VertexInfos^[i] do begin
    for j:=0 to NumNeighbors-1 do begin
     point:=Neighbors^[j].index;
     count:=0;
     for k:=0 to NumFaces-1 do begin
      with Triangles^[Faces^[k]] do begin
       if (a=point) or (b=point) or (c=point) then begin
        inc(count);
       end;
      end;
     end;
     if count<2 then begin
      Distance:=INFINITY+1;
     end;
    end;
   end;
  end;
 end;
 procedure ProgressiveMesh;
 var i,n,j,k:integer;
 begin
  Vertices:=nil;
  Triangles:=nil;
  NumVertices:=0;
  NumTriangles:=Mesh.NumTriangles;
  getmem(Triangles,NumTriangles*sizeof(TTriangle));
  fillchar(Triangles^,NumTriangles*sizeof(TTriangle),#0);
  for i:=0 to Mesh.NumTriangles-1 do begin
   for n:=0 to 2 do begin
    k:=-1;
    for j:=0 to NumVertices-1 do begin
     if Vector3Compare(Vertices^[j],Vector3(Mesh.Triangles^[i].Vertices[n])) then begin
      k:=j;
      break;
     end;
    end;
    if k<0 then begin
     k:=NumVertices;
     inc(NumVertices);          
     PhysicsReallocateMemory(Vertices,NumVertices*sizeof(TVertex));
     Vertices^[k]:=Vector3(Mesh.Triangles^[i].Vertices[n]);
    end;
    case n of
     0:Triangles^[i].a:=k;
     1:Triangles^[i].b:=k;
     2:Triangles^[i].c:=k;
    end;
   end;
  end;
  NumVertexInfos:=NumVertices;
  getmem(VertexInfos,NumVertexInfos*sizeof(TVertexInfo));
  fillchar(VertexInfos^,NumVertexInfos*sizeof(TVertexInfo),#0);
  NumFaceInfos:=NumTriangles;
  getmem(FaceInfos,NumFaceInfos*sizeof(TFaceInfo));
  fillchar(FaceInfos^,NumFaceInfos*sizeof(TFaceInfo),#0);
  for i:=0 to NumTriangles-1 do begin
   with Triangles^[i] do begin
    AddTriangle(a,i);
    AddTriangle(b,i);
    AddTriangle(c,i);
   end;
   ComputeNormal(i);
  end;
  FreezeBorders;
  for i:=0 to NumVertices-1 do begin
   ComputeVertexCost(i);
  end;
  if assigned(Mesh.Triangles) then begin
   freemem(Mesh.Triangles);
   Mesh.Triangles:=nil;
  end;
  Mesh.NumTriangles:=0;
  Mesh.AllTriangles:=0;
  if lod<0 then begin
   lod:=(NumVertices*abs(lod)) div 100;
  end;
  if lod>=NumVertices then begin
   lod:=NumVertices-1;
  end;
  j:=0;
  for i:=0 to LOD-1 do begin
   inc(j,CollapseVertex);
  end;
  j:=NumTriangles-j;
  n:=0;
  for i:=0 to NumTriangles-1 do begin
   if not FaceInfos[i].Collapsed then begin
    if n<j then begin
     PhysicsObjectMeshAddTriangle(Mesh,Vector3Ex(Vertices^[Triangles^[i].a]),Vector3Ex(Vertices^[Triangles^[i].b]),Vector3Ex(Vertices^[Triangles^[i].c]));
     inc(n);
    end;
   end;
  end;
  if assigned(VertexInfos) then begin
   freemem(VertexInfos);
  end;
  if assigned(FaceInfos) then begin
   freemem(FaceInfos);
  end;
  if assigned(Triangles) then begin
   freemem(Triangles);
  end;
  if assigned(Vertices) then begin
   freemem(Vertices);
  end;
 end;
var i:integer;
begin
 for i:=0 to Mesh.NumMeshs-1 do begin
  PhysicsObjectMeshOptimizeToLevelOfDetail(Mesh.Meshs^[i]^,LOD);
 end;
 ProgressiveMesh;
end;

const NextEdge:array[0..2] of integer=(1,2,0);
      NextVertex:array[0..2] of integer=(1,2,0);

function PhysicsObjectGenerateConvexHull(var Instance:TPhysicsObject):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const EPSILON=1e-10;
      DOUBLE_PREC:double=2.2204460492503131e-16;
type TFloat={$ifdef cpu386}extended{$else}double{$endif};
     TVector3=packed record
      x,y,z:TFloat;
     end;
     PVertex=^TVertex;
     TVertex=record
      Previous,Next:PVertex;
      Vector:TVector3;
      RandomOrderIndexValue:longword;
     end;
     TPlane=packed record
      case boolean of
       false:(a,b,c,d:TFloat);
       true:(Normal:TVector3;
             Distance:TFloat);
     end;
     PPVertices=^TPVertices;
     TPVertices=array[0..65535] of PVertex;
     PTriangle=^TTriangle;
     TTriangle=record
      Previous,Next:PTriangle;
      Vertices:array[0..2] of TVector3;
      Plane:TPlane;
     end;
     PEdge=^TEdge;
     TEdge=record
      Previous,Next:PEdge;
      Vertices:array[0..1] of TVector3;
     end;
var VerticesFirst,VerticesLast:PVertex;
    TrianglesFirst,TrianglesLast:PTriangle;
    EdgesFirst,EdgesLast:PEdge;
    Vertices:PPVertices;
    VerticesCount,TrianglesCount:integer;
    RandomSeed:longword;
    MinVector,MaxVector:TVector3;
    Tolerance:TFloat;
 function GetRandomValue:longword;
 begin
  RandomSeed:=(RandomSeed*1664525)+1013904223;
  result:=RandomSeed;
 end;
 function Vector3Compare(const v1,v2:TVector3):boolean;
 begin
  result:=(abs(v1.x-v2.x)<EPSILON) and (abs(v1.y-v2.y)<EPSILON) and (abs(v1.z-v2.z)<EPSILON);
 end;
 function Vector3Sub(const v1,v2:TVector3):TVector3;
 begin
  result.x:=v1.x-v2.x;
  result.y:=v1.y-v2.y;
  result.z:=v1.z-v2.z;
 end;
 function Vector3Add(const v1,v2:TVector3):TVector3;
 begin
  result.x:=v1.x+v2.x;
  result.y:=v1.y+v2.y;
  result.z:=v1.z+v2.z;
 end;
 function Vector3Cross(const v1,v2:TVector3):TVector3;
 begin
  result.x:=(v1.y*v2.z)-(v1.z*v2.y);
  result.y:=(v1.z*v2.x)-(v1.x*v2.z);
  result.z:=(v1.x*v2.y)-(v1.y*v2.x);
 end;
 function Vector3Length(const v:TVector3):TFloat;
 begin
  result:=sqrt(sqr(v.x)+sqr(v.y)+sqr(v.z));
 end;
 function Vector3LengthSquared(const v:TVector3):TFloat;
 begin
  result:=sqr(v.x)+sqr(v.y)+sqr(v.z);
 end;
 function Vector3ScalarMul(const v:TVector3;s:TFloat):TVector3;
 begin
  result.x:=v.x*s;
  result.y:=v.y*s;
  result.z:=v.z*s;
 end;
 function Vector3Dot(const v1,v2:TVector3):TFloat;
 begin
  result:=(v1.x*v2.x)+(v1.y*v2.y)+(v1.z*v2.z);
 end;
 function Vector3Norm(const v:TVector3):TVector3;
 var l:TFloat;
 begin
  l:=Vector3Length(v);
  if abs(l)<EPSILON then begin
   result.x:=0;
   result.y:=0;
   result.z:=0;
  end else begin
   result:=Vector3ScalarMul(v,1/l);
  end;
 end;
 function Vector3Dist(const v1,v2:TVector3):TFloat;
 begin
  result:=Vector3Length(Vector3Sub(v2,v1));
 end;
 function Vector3(v:TPhysicsVector3):TVector3;
 begin
  result.x:=v.x;
  result.y:=v.y;
  result.z:=v.z;
 end;
 function Vector3Ex(v:TVector3):TPhysicsVector3;
 begin
  result.x:=v.x;
  result.y:=v.y;
  result.z:=v.z;
 end;
 function PlaneFromPoints(P1,P2,P3:TVector3):TPlane;
 var n:TVector3;
 begin
  n:=Vector3Norm(Vector3Cross(Vector3Sub(P2,P1),Vector3Sub(P3,P1)));
  result.a:=n.x;
  result.b:=n.y;
  result.c:=n.z;
  result.d:=-((result.a*P1.x)+(result.b*P1.y)+(result.c*P1.z));
 end;
 function PlaneVectorDistance(const Plane:TPlane;const Point:TVector3):TFloat;
 begin
  result:=(Plane.a*Point.x)+(Plane.b*Point.y)+(Plane.c*Point.z)+Plane.d;
 end;
 function CalculateArea(const v0,v1,v2:TVector3):TFloat;
 begin
  result:=Vector3LengthSquared(Vector3Cross(Vector3Sub(v1,v0),Vector3Sub(v2,v0)));
 end;
 function CalculateVolume(const v0,v1,v2,v3:TVector3):TFloat;
 var a,b,c:TVector3;
 begin
  a:=Vector3Sub(v0,v3);
  b:=Vector3Sub(v1,v3);
  c:=Vector3Sub(v2,v3);
  result:=(a.x*((b.z*c.y)-(b.y*c.z)))+(a.y*((b.x*c.z)-(b.z*c.x)))+(a.z*((b.y*c.x)-(b.x*c.y)));
 end;
 function FindVertex(const Vector:TVector3):PVertex;
 var Vertex:PVertex;
 begin
  result:=nil;
  Vertex:=VerticesFirst;
  while assigned(Vertex) do begin
   if Vector3Compare(Vertex^.Vector,Vector) then begin
    result:=Vertex;
    break;
   end;
   Vertex:=Vertex^.Next;
  end;
 end;
 function AddVertex(const Vector:TVector3):PVertex;
 begin
  new(result);
  fillchar(result^,sizeof(TVertex),#0);
  result^.Vector:=Vector;
  result^.RandomOrderIndexValue:=GetRandomValue;
  if assigned(VerticesLast) then begin
   result^.Previous:=VerticesLast;
   result^.Next:=nil;
   VerticesLast^.Next:=result;
   VerticesLast:=result;
  end else begin
   result^.Previous:=nil;
   result^.Next:=nil;
   VerticesFirst:=result;
   VerticesLast:=result;
  end;
 end;
 procedure DeleteVertex(Vertex:PVertex);
 begin
  if assigned(Vertex^.Next) then begin
   Vertex^.Next^.Previous:=Vertex^.Previous;
  end;
  if assigned(Vertex^.Previous) then begin
   Vertex^.Previous^.Next:=Vertex^.Next;
  end;
  if VerticesFirst=Vertex then begin
   VerticesFirst:=Vertex^.Next;
  end;
  if VerticesLast=Vertex then begin
   VerticesLast:=Vertex^.Previous;
  end;
  dispose(Vertex);
 end;
 function AddTriangle(const v0,v1,v2:TVector3):PTriangle;
 begin
  new(result);
  fillchar(result^,sizeof(TTriangle),#0);
  result^.Vertices[0]:=v0;
  result^.Vertices[1]:=v1;
  result^.Vertices[2]:=v2;
  result^.Plane:=PlaneFromPoints(v0,v1,v2);
  if assigned(TrianglesLast) then begin
   result^.Previous:=TrianglesLast;
   result^.Next:=nil;
   TrianglesLast^.Next:=result;
   TrianglesLast:=result;
  end else begin
   result^.Previous:=nil;
   result^.Next:=nil;
   TrianglesFirst:=result;
   TrianglesLast:=result;
  end;
  inc(TrianglesCount);
 end;
 procedure DeleteTriangle(Triangle:PTriangle);
 begin
  if assigned(Triangle^.Next) then begin
   Triangle^.Next^.Previous:=Triangle^.Previous;
  end;
  if assigned(Triangle^.Previous) then begin
   Triangle^.Previous^.Next:=Triangle^.Next;
  end;
  if TrianglesFirst=Triangle then begin
   TrianglesFirst:=Triangle^.Next;
  end;
  if TrianglesLast=Triangle then begin
   TrianglesLast:=Triangle^.Previous;
  end;
  dispose(Triangle);
  dec(TrianglesCount);
 end;
 procedure DeleteEdge(Edge:PEdge);
 begin
  if assigned(Edge^.Next) then begin
   Edge^.Next^.Previous:=Edge^.Previous;
  end;
  if assigned(Edge^.Previous) then begin
   Edge^.Previous^.Next:=Edge^.Next;
  end;
  if EdgesFirst=Edge then begin
   EdgesFirst:=Edge^.Next;
  end;
  if EdgesLast=Edge then begin
   EdgesLast:=Edge^.Previous;
  end;
  dispose(Edge);
 end;
 function AddEdge(const v0,v1:TVector3):PEdge;
 var Edge,FoundEdge:PEdge;
 begin
  result:=nil;
  FoundEdge:=nil;
  Edge:=EdgesFirst;
  while assigned(Edge) do begin
   if Vector3Compare(Edge^.Vertices[0],v1) and Vector3Compare(Edge^.Vertices[1],v0) then begin
    FoundEdge:=Edge;
    break;
   end;
   Edge:=Edge^.Next;
  end;
  if assigned(FoundEdge) then begin
   DeleteEdge(Edge);
  end else begin
   new(result);
   fillchar(result^,sizeof(TEdge),#0);
   result^.Vertices[0]:=v0;
   result^.Vertices[1]:=v1;
   if assigned(EdgesLast) then begin
    result^.Previous:=EdgesLast;
    result^.Next:=nil;
    EdgesLast^.Next:=result;
    EdgesLast:=result;
   end else begin
    result^.Previous:=nil;
    result^.Next:=nil;
    EdgesFirst:=result;
    EdgesLast:=result;
   end;
  end;
 end;
 procedure SearchMinMaxVector;
 var Vertex:PVertex;
 begin
  if assigned(VerticesFirst) then begin
   MinVector:=VerticesFirst^.Vector;
   MaxVector:=VerticesFirst^.Vector;
   Vertex:=VerticesFirst^.Next;
   while assigned(Vertex) do begin
    if MinVector.x>Vertex^.Vector.x then begin
     MinVector.x:=Vertex^.Vector.x;
    end;
    if MinVector.y>Vertex^.Vector.y then begin
     MinVector.y:=Vertex^.Vector.y;
    end;
    if MinVector.z>Vertex^.Vector.z then begin
     MinVector.z:=Vertex^.Vector.z;
    end;
    if MaxVector.x<Vertex^.Vector.x then begin
     MaxVector.x:=Vertex^.Vector.x;
    end;
    if MaxVector.y<Vertex^.Vector.y then begin
     MaxVector.y:=Vertex^.Vector.y;
    end;
    if MaxVector.z<Vertex^.Vector.z then begin
     MaxVector.z:=Vertex^.Vector.z;
    end;
    Vertex:=Vertex^.Next;
   end;
  end else begin
   fillchar(MinVector,sizeof(TVector3),#0);
   fillchar(MaxVector,sizeof(TVector3),#0);
  end;
 end;
 procedure CalculateTolerance;
 begin
  Tolerance:=3*DOUBLE_PREC*(abs(MaxVector.x-MinVector.x)+abs(MaxVector.y-MinVector.y)+abs(MaxVector.z-MinVector.z));
 end;
 procedure CollectMeshVertices(var Mesh:TPhysicsObjectMesh);
 var i,j:integer;
 begin
  for i:=0 to Mesh.NumMeshs-1 do begin
   CollectMeshVertices(Mesh.Meshs^[i]^);
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   for j:=0 to 1 do begin
    if not assigned(FindVertex(Vector3(Mesh.Triangles^[i].Vertices[j]))) then begin
     AddVertex(Vector3(Mesh.Triangles^[i].Vertices[j]));
     inc(VerticesCount);
    end;
   end;
  end;
 end;
 procedure CollectVertices;
 var i:integer;
 begin
  VerticesCount:=0;
  for i:=0 to Instance.NumMeshs-1 do begin
   CollectMeshVertices(Instance.Meshs^[i]^);
  end;
 end;
 procedure CollectMeshTriangles(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  for i:=0 to Mesh.NumMeshs-1 do begin
   CollectMeshTriangles(Mesh.Meshs^[i]^);
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   AddTriangle(Vector3(Mesh.Triangles^[i].Vertices[0]),Vector3(Mesh.Triangles^[i].Vertices[1]),Vector3(Mesh.Triangles^[i].Vertices[2]));
  end;
 end;
 procedure CollectTriangles;
 var i:integer;
 begin
  TrianglesCount:=0;
  for i:=0 to Instance.NumMeshs-1 do begin
   CollectMeshTriangles(Instance.Meshs^[i]^);
  end;
 end;
 procedure Init;
 begin
  PhysicsObjectMeshDone(Instance.ConvexHullMesh);
  VerticesFirst:=nil;
  VerticesLast:=nil;
  TrianglesFirst:=nil;
  TrianglesLast:=nil;
  EdgesFirst:=nil;
  EdgesLast:=nil;
  Vertices:=nil;
  RandomSeed:=longword(longint((Instance.NumMeshs*Instance.NumMeshs)+Instance.NumMeshs)) xor $c001babe;
 end;
 procedure Done;
 begin
  if assigned(Vertices) then begin
   freemem(Vertices);
  end;
  while assigned(VerticesLast) do begin
   DeleteVertex(VerticesLast);
  end;
  while assigned(TrianglesLast) do begin
   DeleteTriangle(TrianglesLast);
  end;
  while assigned(EdgesLast) do begin
   DeleteEdge(EdgesLast);
  end;
 end;
 procedure ClearTriangles;
 begin
  while assigned(TrianglesLast) do begin
   DeleteTriangle(TrianglesLast);
  end;
  TrianglesCount:=0;
 end;
 procedure SortVerticesRandomized;
 var PartA,PartB,Vertex:PVertex;
     InSize,PartASize,PartBSize,Merges:integer;
 begin
  if assigned(VerticesFirst) then begin
   InSize:=1;
   while true do begin
    PartA:=VerticesFirst;
    VerticesFirst:=nil;
    VerticesLast:=nil;
    Merges:=0;
    while assigned(PartA) do begin
     inc(Merges);
     PartB:=PartA;
     PartASize:=0;
     while PartASize<InSize do begin
      inc(PartASize);
      PartB:=PartB^.Next;
      if not assigned(PartB) then begin
       break;
      end;
     end;
     PartBSize:=InSize;
     while (PartASize>0) or ((PartBSize>0) and assigned(PartB)) do begin
      if PartASize=0 then begin
       Vertex:=PartB;
       PartB:=PartB^.Next;
       dec(PartBSize);
      end else if (PartBSize=0) or not assigned(PartB) then begin
       Vertex:=PartA;
       PartA:=PartA^.Next;
       dec(PartASize);
      end else if PartA^.RandomOrderIndexValue<=PartB^.RandomOrderIndexValue then begin
       Vertex:=PartA;
       PartA:=PartA^.Next;
       dec(PartASize);
      end else begin
       Vertex:=PartB;
       PartB:=PartB^.Next;
       dec(PartBSize);
      end;
      if assigned(VerticesLast) then begin
       VerticesLast^.Next:=Vertex;
      end else begin
       VerticesFirst:=Vertex;
      end;
      Vertex^.Previous:=VerticesLast;
      VerticesLast:=Vertex;
     end;
     PartA:=PartB;
    end;
    VerticesLast^.Next:=nil;
    if Merges<=1 then begin
     break;
    end;
    inc(InSize,InSize);
   end;
  end;
 end;
 procedure GenerateVerticesArray;
 var i:integer;
     Vertex:PVertex;
 begin
  if assigned(Vertices) then begin
   freemem(Vertices);
  end;
  getmem(Vertices,VerticesCount*sizeof(PVertex));
  i:=0;
  Vertex:=VerticesFirst;
  while assigned(Vertex) do begin
   Vertices^[i]:=Vertex;
   inc(i);
   Vertex:=Vertex^.Next;
  end;
 end;
 procedure SortVerticesArray;
  function CompareFunction(const a,b:PVertex):integer;
  begin
   if (a^.Vector.x>b^.Vector.x) or
      ((a^.Vector.x=b^.Vector.x) and (a^.Vector.y>b^.Vector.y)) or
      ((a^.Vector.x=b^.Vector.x) and (a^.Vector.y=b^.Vector.y) and (a^.Vector.z>b^.Vector.z)) then begin
    result:=-1;
   end else if (a^.Vector.x<b^.Vector.x) or
               ((a^.Vector.x=b^.Vector.x) and (a^.Vector.y<b^.Vector.y)) or
               ((a^.Vector.x=b^.Vector.x) and (a^.Vector.y=b^.Vector.y) and (a^.Vector.z<b^.Vector.z)) then begin
    result:=1;
   end else begin
    result:=0;
   end;
  end;
  procedure ProcessSort(Left,Right:integer;Depth:longword);
   procedure SiftDown(Current,MaxIndex:integer);
   var SiftLeft,SiftRight,Largest:Integer;
       t:pointer;
   begin
    SiftLeft:=Left+(2*(Current-Left))+1;
    SiftRight:=Left+(2*(Current-Left))+2;
    Largest:=Current;
    if (SiftLeft<=MaxIndex) and (CompareFunction(Vertices^[SiftLeft],Vertices^[Largest])>0) then begin
     Largest:=SiftLeft;
    end;
    if (SiftRight<=MaxIndex) and (CompareFunction(Vertices^[SiftRight],Vertices^[Largest])>0) then begin
     Largest:=SiftRight;
    end;
    if Largest<>Current then begin
     t:=Vertices^[Current];
     Vertices^[Current]:=Vertices^[Largest];
     Vertices^[Largest]:=t;
     SiftDown(Largest,MaxIndex);
    end;
   end;
  var Middle,i,j:integer;
      x,t:pointer;
  begin
   if Left>Right then begin
    exit;
   end;
   if (Right-Left)<16 then begin
    // Insertion sort
    for i:=Left+1 to Right do begin
     t:=Vertices^[i];
     j:=i-1;
     while (j>=Left) and (CompareFunction(t,Vertices^[j])<0) do begin
      Vertices^[j+1]:=Vertices^[j];
      dec(j);
     end;
     Vertices^[j+1]:=t;
    end;
   end else if Depth=0 then begin
    // Heap sort
    for i:=((Left+Right+1) div 2)-1 downto Left do begin
     SiftDown(i,Right);
    end;
    for i:=Right downto Left+1 do begin
     t:=Vertices^[i];
     Vertices^[i]:=Vertices^[Left];
     Vertices^[Left]:=t;
     SiftDown(Left,i-1);
    end;
   end else begin
    // Quick sort with median of three
    Middle:=(Left+Right) div 2;
    if CompareFunction(Vertices^[Left],Vertices^[Middle])>0 then begin
     t:=Vertices^[Left];
     Vertices^[Left]:=Vertices^[Middle];
     Vertices^[Middle]:=t;
    end;
    if CompareFunction(Vertices^[Left],Vertices^[Right])>0 then begin
     t:=Vertices^[Left];
     Vertices^[Left]:=Vertices^[Right];
     Vertices^[Right]:=t;
    end;
    if CompareFunction(Vertices^[Middle],Vertices^[Right])>0 then begin
     t:=Vertices^[Middle];
     Vertices^[Middle]:=Vertices^[Right];
     Vertices^[Right]:=t;
    end;
    t:=Vertices^[Middle];
    Vertices^[Middle]:=Vertices^[Right-1];
    Vertices^[Right-1]:=t;
    x:=t;
    i:=Left;                           
    j:=Right-1;
    while true do begin
     repeat
      inc(i);
     until not ((i<Right) and (CompareFunction(Vertices^[i],x)<0));
     repeat
      dec(j);
     until not ((j>Left) and (CompareFunction(Vertices^[j],x)>0));
     if i>=j then begin
      break;
     end else begin
      t:=Vertices^[i];
      Vertices^[i]:=Vertices^[j];
      Vertices^[j]:=t;
     end;
    end;
    t:=Vertices^[i];
    Vertices^[i]:=Vertices^[Right-1];
    Vertices^[Right-1]:=t;
    ProcessSort(Left,i-1,Depth-1);
    ProcessSort(i+1,Right,Depth-1);
   end;
  end;
 begin
  if VerticesCount>0 then begin
   ProcessSort(0,VerticesCount-1,IntLog2(VerticesCount)*2);
  end;
 end;
 function IsValidExactConvexHull:boolean;
 var Triangle:PTriangle;
     Vertex:PVertex;
     PointOutside:boolean;
 begin
  result:=true;
  Triangle:=TrianglesFirst;
  while assigned(Triangle) do begin
   Vertex:=VerticesFirst;
   while assigned(Vertex) do begin
    if PhysicsInstance^.ConvexHullGenerationPlaneSideCheckAtTest then begin
     PointOutside:=PlaneVectorDistance(Triangle^.Plane,Vertex^.Vector)>Tolerance;
    end else begin
     PointOutside:=CalculateVolume(Triangle^.Vertices[0],Triangle^.Vertices[1],Triangle^.Vertices[2],Vertex^.Vector)>0;
    end;
    if PointOutside then begin
     result:=false;
     exit;
    end;
    Vertex:=Vertex^.Next;
   end;
   Triangle:=Triangle^.Next;
  end;
 end;
 function FindAndAddFirstTetrahedron:boolean;
 var LowestVertex,HighestVertex,OtherVertex,YetOtherVertex,Vertex:PVertex;
     BestArea,Area,BestVolume,Volume:TFloat;
     a:TVector3;
     Vertices:array[0..3] of PVertex;
     i:integer;
     Triangle:PTriangle;
 begin
  result:=false;
  LowestVertex:=VerticesFirst;
  HighestVertex:=VerticesFirst;
  Vertex:=VerticesFirst;
  while assigned(Vertex) do begin
   if (Vertex^.Vector.x<LowestVertex^.Vector.x) or
      ((Vertex^.Vector.x=LowestVertex^.Vector.x) and (Vertex^.Vector.y<LowestVertex^.Vector.y)) or
      ((Vertex^.Vector.x=LowestVertex^.Vector.x) and (Vertex^.Vector.y=LowestVertex^.Vector.y) and (Vertex^.Vector.z<LowestVertex^.Vector.z)) then begin
    LowestVertex:=Vertex;
   end;
   if (Vertex^.Vector.x>LowestVertex^.Vector.x) or
      ((Vertex^.Vector.x=LowestVertex^.Vector.x) and (Vertex^.Vector.y>LowestVertex^.Vector.y)) or
      ((Vertex^.Vector.x=LowestVertex^.Vector.x) and (Vertex^.Vector.y=LowestVertex^.Vector.y) and (Vertex^.Vector.z>LowestVertex^.Vector.z)) then begin
    HighestVertex:=Vertex;
   end;
   Vertex:=Vertex^.Next;
  end;
  if LowestVertex=HighestVertex then begin
   exit;
  end;
  OtherVertex:=VerticesFirst;
  Vertex:=VerticesFirst;
  BestArea:=0;
  while assigned(Vertex) do begin
   Area:=abs(CalculateArea(LowestVertex^.Vector,HighestVertex^.Vector,Vertex^.Vector));
   if BestArea<Area then begin
    BestArea:=Area;
    OtherVertex:=Vertex;
   end;
   Vertex:=Vertex^.Next;
  end;
  if (OtherVertex=LowestVertex) or (OtherVertex=HighestVertex) then begin
   exit;
  end;
  YetOtherVertex:=VerticesFirst;
  Vertex:=VerticesFirst;
  BestVolume:=0;
  while assigned(Vertex) do begin
   Volume:=abs(CalculateVolume(LowestVertex^.Vector,OtherVertex^.Vector,HighestVertex^.Vector,YetOtherVertex^.Vector));
   if BestVolume<Volume then begin
    BestVolume:=Volume;
    YetOtherVertex:=Vertex;
   end;
   Vertex:=Vertex^.Next;
  end;
  if (YetOtherVertex=LowestVertex) or (YetOtherVertex=HighestVertex) or (YetOtherVertex=OtherVertex) then begin
   exit;
  end;
  Vertices[0]:=LowestVertex;
  Vertices[1]:=OtherVertex;
  Vertices[2]:=YetOtherVertex;
  Vertices[3]:=HighestVertex;
  for i:=0 to 3 do begin
   Triangle:=AddTriangle(Vertices[i and 3].Vector,Vertices[(i+1) and 3].Vector,Vertices[(i+2) and 3].Vector);
   if CalculateVolume(Triangle^.Vertices[0],Triangle^.Vertices[1],Triangle^.Vertices[2],Vertices[(i+3) and 3]^.Vector)>0 then begin
    a:=Triangle^.Vertices[0];
    Triangle^.Vertices[0]:=Triangle^.Vertices[1];
    Triangle^.Vertices[1]:=a;
   end;
  end;
  result:=true;
 end;
 procedure CopyIntoMesh;
 var Triangle:PTriangle;
 begin
  Triangle:=TrianglesFirst;
  while assigned(Triangle) do begin
   PhysicsObjectMeshAddTriangle(Instance.ConvexHullMesh,Vector3Ex(Triangle^.Vertices[0]),Vector3Ex(Triangle^.Vertices[1]),Vector3Ex(Triangle^.Vertices[2]));
   Triangle:=Triangle^.Next;
  end;
  if PhysicsInstance^.ConvexHullGenerationLevelOfDetail>=0 then begin
   PhysicsObjectMeshOptimizeToLevelOfDetail(Instance.ConvexHullMesh,PhysicsInstance^.ConvexHullGenerationLevelOfDetail);
  end;
  PhysicsObjectMeshSubdivide(Instance.ConvexHullMesh);
  PhysicsObjectMeshFinish(Instance.ConvexHullMesh);
 end;
 procedure AddPoint(const PointVector:TVector3);
 var Outside,PointOutside:boolean;
     Triangle,NextTriangle:PTriangle;
 begin
  Outside:=false;
  Triangle:=TrianglesFirst;
  while assigned(Triangle) do begin
   NextTriangle:=Triangle^.Next;
   if PhysicsInstance^.ConvexHullGenerationPlaneSideCheckAtGeneration then begin
    PointOutside:=PlaneVectorDistance(Triangle^.Plane,PointVector)>Tolerance;
   end else begin
    PointOutside:=CalculateVolume(Triangle^.Vertices[0],Triangle^.Vertices[1],Triangle^.Vertices[2],PointVector)>0;
   end;
   if PointOutside then begin
    Outside:=true;
    AddEdge(Triangle^.Vertices[0],Triangle^.Vertices[1]);
    AddEdge(Triangle^.Vertices[1],Triangle^.Vertices[2]);
    AddEdge(Triangle^.Vertices[2],Triangle^.Vertices[0]);
    DeleteTriangle(Triangle);
   end;
   Triangle:=NextTriangle;
  end;
  if Outside then begin
   while assigned(EdgesLast) do begin
    AddTriangle(EdgesLast^.Vertices[0],EdgesLast^.Vertices[1],PointVector);
    DeleteEdge(EdgesLast);
   end;
  end;
 end;
 procedure BEROHullBuild(Lowest,Highest:integer);
 var Split,i:integer;
     Area,BestArea:TFloat;
 begin
  if Lowest<=Highest then begin
   if (Highest-Lowest)<4 then begin
    for i:=Lowest to Highest do begin
     AddPoint(Vertices^[i].Vector);
    end;
   end else begin
    Split:=(Lowest+Highest) div 2;
    if PhysicsInstance^.ConvexHullGenerationWorkMode=chgwmBEROHULL then begin
     BestArea:=0;
     for i:=Lowest+1 to Highest-1 do begin
      Area:=abs(CalculateArea(Vertices^[Lowest].Vector,Vertices^[Highest].Vector,Vertices^[i].Vector));
      if BestArea<Area then begin
       BestArea:=Area;
       Split:=i;
      end;
     end;
    end;
    AddPoint(Vertices^[Lowest].Vector);
    AddPoint(Vertices^[Split].Vector);
    AddPoint(Vertices^[Highest].Vector);
    BEROHullBuild(Lowest+1,Split-1);
    BEROHullBuild(Split+1,Highest-1);
   end;
  end;
 end;
 procedure Build;
 var Vertex:PVertex;
 begin
  if VerticesCount=3 then begin
   AddTriangle(VerticesFirst^.Vector,VerticesFirst^.Next^.Vector,VerticesFirst^.Next^.Next^.Vector);
  end else if VerticesCount>=4 then begin
   if PhysicsInstance^.ConvexHullGenerationWorkMode=chgwmRANDOMIZEDINCREMENTAL then begin
    SortVerticesRandomized;
   end;
   if FindAndAddFirstTetrahedron then begin
    Vertex:=VerticesFirst;
   end else begin
    AddTriangle(VerticesFirst^.Vector,VerticesFirst^.Next^.Vector,VerticesFirst^.Next^.Next^.Vector);
    AddTriangle(VerticesFirst^.Vector,VerticesFirst^.Next^.Next^.Vector,VerticesFirst^.Next^.Vector);
    Vertex:=VerticesFirst^.Next^.Next^.Next;
   end;
   if PhysicsInstance^.ConvexHullGenerationWorkMode in [chgwmSIMPLEBEROHULL,chgwmBEROHULL] then begin
    GenerateVerticesArray;
    SortVerticesArray;
    BEROHullBuild(0,VerticesCount-1);
   end else begin
    while assigned(Vertex) do begin
     AddPoint(Vertex^.Vector);
     Vertex:=Vertex^.Next;
    end;
   end;
  end;
 end;
begin
 Init;
 CollectVertices;
 SearchMinMaxVector;
 CalculateTolerance;
 CollectTriangles;
 result:=IsValidExactConvexHull;
 if not result then begin
  ClearTriangles;
  Build;
  if not assigned(TrianglesFirst) then begin
   CollectTriangles;
  end;
  result:=IsValidExactConvexHull;
 end;
 CopyIntoMesh;
 Done;
end;

procedure PhysicsObjectFinish(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    First:boolean;
    CylinderRadius,CylinderRadiusSqrt:TPhysicsFloat;
    ExtendVector:TPhysicsVector3;
 procedure AABBCheckMesh(var AABB:TPhysicsAABB;var Mesh:TPhysicsObjectMesh;var First:boolean);
 var i,j:integer;
     v:TPhysicsVector3;
 begin
  for i:=0 to Mesh.NumTriangles-1 do begin
   for j:=0 to 2 do begin
    v:=Mesh.Triangles^[i].Vertices[j];
    if First then begin
     First:=false;
     AABB.Min:=v;
     AABB.Max:=v;
    end else begin
     if AABB.Min.x>v.x then AABB.Min.x:=v.x;
     if AABB.Min.y>v.y then AABB.Min.y:=v.y;
     if AABB.Min.z>v.z then AABB.Min.z:=v.z;
     if AABB.Max.x<v.x then AABB.Max.x:=v.x;
     if AABB.Max.y<v.y then AABB.Max.y:=v.y;
     if AABB.Max.z<v.z then AABB.Max.z:=v.z;
    end;
   end;
  end;
  for i:=0 to Mesh.NumMeshs-1 do begin
   AABBCheckMesh(AABB,Mesh.Meshs^[i]^,First);
  end;
 end;
 procedure SphereCheckMesh(var Sphere:TPhysicsSphere;var Mesh:TPhysicsObjectMesh);
 var i,j:integer;
     v:TPhysicsVector3;
     r:TPhysicsFloat;
 begin
  for i:=0 to Mesh.NumTriangles-1 do begin
   for j:=0 to 2 do begin
    v:=Mesh.Triangles^[i].Vertices[j];
    r:=Vector3Length(Vector3Sub(Sphere.Center,v));
    if Sphere.Radius<r then begin
     Sphere.Radius:=r;
    end;
   end;
  end;
  for i:=0 to Mesh.NumMeshs-1 do begin
   SphereCheckMesh(Sphere,Mesh.Meshs^[i]^);
  end;
 end;
 procedure CreateBoxMesh(var MeshInstance:TPhysicsObjectMesh;MinVec,MaxVec:TPhysicsVector3);
 var v:array[0..7] of TPhysicsVector3;
 begin
  v[0]:=Vector3(MinVec.X,MinVec.Y,MinVec.Z);
  v[1]:=Vector3(MaxVec.X,MinVec.Y,MinVec.Z);
  v[2]:=Vector3(MaxVec.X,MinVec.Y,MaxVec.Z);
  v[3]:=Vector3(MinVec.X,MinVec.Y,MaxVec.Z);
  v[4]:=Vector3(MinVec.X,MaxVec.Y,MinVec.Z);
  v[5]:=Vector3(MaxVec.X,MaxVec.Y,MinVec.Z);
  v[6]:=Vector3(MaxVec.X,MaxVec.Y,MaxVec.Z);
  v[7]:=Vector3(MinVec.X,MaxVec.Y,MaxVec.Z);
  MeshInstance.NumTriangles:=0;
  PhysicsObjectMeshAddQuad(MeshInstance,v[4],v[5],v[1],v[0]);
  PhysicsObjectMeshAddQuad(MeshInstance,v[5],v[6],v[2],v[1]);
  PhysicsObjectMeshAddQuad(MeshInstance,v[6],v[7],v[3],v[2]);
  PhysicsObjectMeshAddQuad(MeshInstance,v[7],v[4],v[0],v[3]);
  PhysicsObjectMeshAddQuad(MeshInstance,v[0],v[1],v[2],v[3]);
  PhysicsObjectMeshAddQuad(MeshInstance,v[7],v[6],v[5],v[4]);
  PhysicsObjectMeshSubdivide(MeshInstance);
  PhysicsObjectMeshFinish(MeshInstance);
 end;
begin
 with Instance do begin
  for i:=0 to NumMeshs-1 do begin
   PhysicsObjectMeshFinish(Meshs^[i]^);
  end;
  if NumMeshs>0 then begin
   ObjectPlane:=Meshs^[0]^.MeshPlane;
  end;
  AABB.Min:=Vector3Origin;
  AABB.Max:=Vector3Origin;
  First:=true;
  for i:=0 to NumMeshs-1 do begin
   AABBCheckMesh(AABB,Meshs^[i]^,First);
  end;
  Sphere.Center:=Vector3ScalarMul(Vector3Add(AABB.Min,AABB.Max),0.5);
  Sphere.Radius:=0;
  for i:=0 to NumMeshs-1 do begin
   SphereCheckMesh(Sphere,Meshs^[i]^);
  end;
  TransformAABB:=AABB;
  TransformSphere:=Sphere;
  TransformSphere.Center:=Vector3Add(TransformSphere.Center,Position);
  PhysicsObjectMeshDone(BoxMesh);
  case CollisionBodyType of
   BodyBox:begin
    CreateBoxMesh(BoxMesh,AABB.Min,AABB.Max);
   end;
   BodyCylinder:begin
    CylinderRadius:=Max(abs(AABB.Max.x-AABB.Min.x),abs(AABB.Max.y-AABB.Min.y))*0.5;
    CylinderRadiusSqrt:=sqrt(Max(CylinderRadius,EPSILON));
    ExtendVector:=Vector3(CylinderRadiusSqrt,CylinderRadiusSqrt,abs(AABB.Max.z-AABB.Min.z)*0.5);
    CreateBoxMesh(BoxMesh,Vector3Neg(ExtendVector),ExtendVector);
   end;
  end;
  if CollisionBodyType=BodyConvexHull then begin
   PhysicsObjectGenerateConvexHull(Instance);
  end else begin
   PhysicsObjectMeshDone(ConvexHullMesh);
  end;
 end;
end;

function PhysicsObjectBoxSegmentIntersect(var Instance:TPhysicsObject;var FracOut:TPhysicsFloat;var PosOut,NormalOut:TPhysicsVector3;Segment:TPhysicsSegment):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var MinValue,MaxValue,e,f,invf,t1,t2,t3:TPhysicsFloat;
    p,h:TPhysicsVector3;
    DirMax,DirMin,Dir:integer;
    MatrixVec:array[0..2] of TPhysicsVector3;
begin
 result:=false;
 FracOut:=INFINITY;
 PosOut:=Vector3Origin;
 NormalOut:=Vector3Origin;
 MinValue:=-INFINITY;
 MaxValue:=INFINITY;
 p:=Vector3Sub(Instance.Position,Segment.Origin);
 h:=Vector3ScalarMul(Vector3Sub(Instance.AABB.Max,Instance.AABB.Min),0.5);
 MatrixVec[0]:=Vector3(Instance.Transform[0,0],Instance.Transform[0,1],Instance.Transform[0,2]);
 MatrixVec[1]:=Vector3(Instance.Transform[1,0],Instance.Transform[1,1],Instance.Transform[1,2]);
 MatrixVec[2]:=Vector3(Instance.Transform[2,0],Instance.Transform[2,1],Instance.Transform[2,2]);
 DirMin:=0;
 DirMax:=0;
 for Dir:=0 to 2 do begin
  e:=Vector3Dot(MatrixVec[Dir],p);
  f:=Vector3Dot(MatrixVec[Dir],Segment.Delta);
  if abs(f)>EPSILON then begin
   invf:=1/f;
   t1:=(e+h.xyz[Dir])*invf;
   t2:=(e-h.xyz[Dir])*invf;
   if t1>t2 then begin
    t3:=t1;
    t1:=t2;
    t2:=t3;
   end;
   if t1>MinValue then begin
    MinValue:=t1;
    DirMin:=Dir;
   end;
   if t2<MaxValue then begin
    MaxValue:=t2;
    DirMax:=Dir;
   end;
   if (MinValue>MaxValue) or (MinValue<0) then exit;
  end else if (((-e)-h.xyz[Dir])>0) or (((-e)+h.xyz[Dir])<0) then begin
   exit;
  end;
 end;
 if MinValue>0 then begin
  Dir:=DirMin;
  FracOut:=MinValue;
 end else begin
  Dir:=DirMax;
  FracOut:=MaxValue;
 end;
 if FracOut<0 then begin
  FracOut:=0;
 end else if FracOut>1 then begin
  FracOut:=1;
 end;
 PosOut:=Vector3Add(Segment.Origin,Vector3ScalarMul(Segment.Delta,FracOut));
 if Vector3Dot(MatrixVec[Dir],Segment.Delta)>0 then begin
  NormalOut:=Vector3Neg(MatrixVec[Dir]);
 end else begin
  NormalOut:=MatrixVec[Dir];
 end;
 result:=true;
end;

function PhysicsObjectHeightMapGetHeight(var Instance:TPhysicsObject;x,y:integer):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if (Instance.HeightMap.Width>0) and (Instance.HeightMap.Height>0) then begin
  if x<0 then begin
   x:=0;
  end else if x>=Instance.HeightMap.Width then begin
   x:=Instance.HeightMap.Width-1;
  end;
  if y<0 then begin
   y:=0;
  end else if y>=Instance.HeightMap.Height then begin
   y:=Instance.HeightMap.Height-1;
  end;
  result:=Instance.HeightMap.Data^[(y*Instance.HeightMap.Width)+x];
 end else begin
  result:=0;
 end;
end;

function PhysicsObjectHeightMapGetNormal(var Instance:TPhysicsObject;x,y:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var x0,x1,y0,y1:integer;
    dx,dz,Left,Right,Forwards,Backwards:TPhysicsFloat;
begin
 if (Instance.HeightMap.Width>0) and (Instance.HeightMap.Height>0) then begin
  if x<0 then begin
   x:=0;
  end else if x>=Instance.HeightMap.Width then begin
   x:=Instance.HeightMap.Width-1;
  end;
  if y<0 then begin
   y:=0;
  end else if y>=Instance.HeightMap.Height then begin
   y:=Instance.HeightMap.Height-1;
  end;
  x0:=x-1;
  x1:=x+1;
  y0:=y-1;
  y1:=y+1;
  if x0<0 then begin
   x0:=0;
  end else if x0>=Instance.HeightMap.Width then begin
   x0:=Instance.HeightMap.Width-1;
  end;
  if y0<0 then begin
   y0:=0;
  end else if y0>=Instance.HeightMap.Height then begin
   y0:=Instance.HeightMap.Height-1;
  end;
  if x1<0 then begin
   x1:=0;
  end else if x1>=Instance.HeightMap.Width then begin
   x1:=Instance.HeightMap.Width-1;
  end;
  if y1<0 then begin
   y1:=0;
  end else if y1>=Instance.HeightMap.Height then begin
   y1:=Instance.HeightMap.Height-1;
  end;
  dx:=(x1-x0)*Instance.HeightMap.CellSize.x;
  dz:=(y1-y0)*Instance.HeightMap.CellSize.z;
  if x0=x1 then dx:=1;
  if y0=y1 then dz:=1;
  if (x0=x1) and (y0=y1) then begin
   result:=Vector3(0,1,0);
   exit;
  end;
  Left:=Instance.HeightMap.Data^[(y*Instance.HeightMap.Width)+x1];
  Right:=Instance.HeightMap.Data^[(y*Instance.HeightMap.Width)+x0];
  Forwards:=Instance.HeightMap.Data^[(y1*Instance.HeightMap.Width)+x];
  Backwards:=Instance.HeightMap.Data^[(y0*Instance.HeightMap.Width)+x];
  result:=Vector3Norm(Vector3Cross(Vector3(dx,Forwards-Backwards,0),Vector3(0,Left-Right,dz)));
 end else begin
  result:=Vector3Origin;
 end;
end;

procedure PhysicsObjectHeightMapGetHeightNormal(var Instance:TPhysicsObject;x,y:integer;var h:TPhysicsFloat;var n:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 h:=PhysicsObjectHeightMapGetHeight(Instance,x,y);
 n:=PhysicsObjectHeightMapGetNormal(Instance,x,y);
end;

function PhysicsObjectHeightMapGetSurfacePos(var Instance:TPhysicsObject;x,y:integer):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Vector3((x*Instance.HeightMap.CellSize.x)-(Instance.HeightMap.Size.x*0.5),PhysicsObjectHeightMapGetHeight(Instance,x,y),(y*Instance.HeightMap.CellSize.z)-(Instance.HeightMap.Size.z*0.5));
end;

procedure PhysicsObjectHeightMapGetSurfacePosAndNormal(var Instance:TPhysicsObject;x,y:integer;var p,n:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 p:=PhysicsObjectHeightMapGetSurfacePos(Instance,x,y);
 n:=PhysicsObjectHeightMapGetNormal(Instance,x,y);
end;

function PhysicsObjectHeightMapGetHeightNormalDistance(var Instance:TPhysicsObject;Point:TPhysicsVector3;var n:TPhysicsVector3):single; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var x,z,xf,zf,h00,h01,h10,h11:TPhysicsFloat;
    x0,y0,x1,y1:integer;
    pl:TPhysicsPlane;
begin
 if (Instance.HeightMap.Width>0) and (Instance.HeightMap.Height>0) then begin
  x:=Min(Max(Point.x,-Instance.HeightMap.Size.x*0.5),Instance.HeightMap.Size.x*0.5)+(Instance.HeightMap.Size.x*0.5);
  z:=Min(Max(Point.z,-Instance.HeightMap.Size.z*0.5),Instance.HeightMap.Size.z*0.5)+(Instance.HeightMap.Size.z*0.5);
  x0:=SoftTRUNC(x/Instance.HeightMap.CellSize.x);
  y0:=SoftTRUNC(z/Instance.HeightMap.CellSize.z);
  x1:=x0+1;
  y1:=y0+1;
  if x0<0 then begin
   x0:=0;
  end else if x0>=Instance.HeightMap.Width then begin
   x0:=Instance.HeightMap.Width-1;
  end;
  if y0<0 then begin
   y0:=0;
  end else if y0>=Instance.HeightMap.Height then begin
   y0:=Instance.HeightMap.Height-1;
  end;
  if x1<0 then begin
   x1:=0;
  end else if x1>=Instance.HeightMap.Width then begin
   x1:=Instance.HeightMap.Width-1;
  end;
  if y1<0 then begin
   y1:=0;
  end else if y1>=Instance.HeightMap.Height then begin
   y1:=Instance.HeightMap.Height-1;
  end;
  xf:=frac(x/Instance.HeightMap.CellSize.x);
  zf:=frac(z/Instance.HeightMap.CellSize.z);
  h00:=Instance.HeightMap.Data^[(y0*Instance.HeightMap.Width)+x0];
  h01:=Instance.HeightMap.Data^[(y1*Instance.HeightMap.Width)+x0];
  h10:=Instance.HeightMap.Data^[(y0*Instance.HeightMap.Width)+x1];
  h11:=Instance.HeightMap.Data^[(y1*Instance.HeightMap.Width)+x1];
  if (x0=x1) and (y0=y1) then begin
   n:=Vector3(0,1,0);
  end else if x0=x1 then begin
   n:=Vector3Norm(Vector3Cross(Vector3(0,h01-h00,Instance.HeightMap.CellSize.z),Vector3(1,0,0)));
  end else if y0=y1 then begin
   n:=Vector3Norm(Vector3Cross(Vector3(0,0,1),Vector3(Instance.HeightMap.CellSize.x,h10-h00,0)));
  end else if xf>zf then begin
   n:=Vector3Norm(Vector3Cross(Vector3(Instance.HeightMap.CellSize.x,h11-h00,Instance.HeightMap.CellSize.z),Vector3(Instance.HeightMap.CellSize.x,h10-h00,0)));
  end else begin
   n:=Vector3Norm(Vector3Cross(Vector3(0,h01-h00,Instance.HeightMap.CellSize.z),Vector3(Instance.HeightMap.CellSize.x,h11-h00,Instance.HeightMap.CellSize.z)));
  end;
  pl.Normal:=n;
  pl.Distance:=-Vector3Dot(n,Vector3((x0*Instance.HeightMap.CellSize.x)-(Instance.HeightMap.Size.x*0.5),h00,(y0*Instance.HeightMap.CellSize.z)-(Instance.HeightMap.Size.z*0.5)));
  result:=PlaneVectorDistance(pl,Point);
 end else begin
  n:=Vector3Origin;
  result:=0;
 end;
end;

function PhysicsObjectHeightMapSegmentIntersect(var Instance:TPhysicsObject;var FracValue:TPhysicsFloat;var Pos,Normal:TPhysicsVector3;Segment:TPhysicsSegment):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var NormalStart,NormalEnd:TPhysicsVector3;
    HeightStart,HeightEnd,DepthEnd,WeightStart,WeightEnd:TPhysicsFloat;
begin
 result:=false;
 FracValue:=0;
 Pos:=Vector3Origin;
 Normal:=Vector3(0,1,0);
 if Segment.Delta.y>-EPSILON then exit;
 HeightStart:=PhysicsObjectHeightMapGetHeightNormalDistance(Instance,Segment.Origin,NormalStart);
 if HeightStart<0 then exit;
 HeightEnd:=PhysicsObjectHeightMapGetHeightNormalDistance(Instance,Vector3Add(Segment.Origin,Segment.Delta),NormalEnd);
 if HeightEnd>0 then exit;
 DepthEnd:=-HeightEnd;
 WeightStart:=1.0/(EPSILON+HeightStart);
 WeightEnd:=1.0/(EPSILON+HeightEnd);
 Normal:=Vector3ScalarMul(Vector3Add(Vector3ScalarMul(NormalStart,WeightStart),Vector3ScalarMul(NormalEnd,WeightEnd)),1.0/(WeightStart+WeightEnd));
 FracValue:=HeightStart/(HeightStart+DepthEnd+EPSILON);
 Pos:=Vector3Add(Segment.Origin,Vector3ScalarMul(Segment.Delta,FracValue));
 result:=true;
end;

procedure PhysicsObjectSetHeightMap(var Instance:TPhysicsObject;Data:pointer;Width,Height:integer;SizeX,SizeZ:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,mi,x,y:integer;
    Quad:TPhysicsQuadVertices;
    Mesh:PPhysicsObjectMesh;
    HalfSize:TPhysicsVector3;
begin
 with Instance do begin
  for i:=0 to NumMeshs-1 do begin
   PhysicsObjectMeshDone(Meshs^[i]^);
   dispose(Meshs^[i]);
   Meshs^[i]:=nil;
  end;
  NumMeshs:=0;
  if assigned(Meshs) then begin
   freemem(Meshs);
   Meshs:=nil;
  end;
  if assigned(HeightMap.Data) then begin
   freemem(HeightMap.Data);
   HeightMap.Data:=nil;
  end;
 end;
 Instance.HeightMap.Width:=Width;
 Instance.HeightMap.Height:=Height;
 Instance.HeightMap.Size.x:=SizeX;
 Instance.HeightMap.Size.y:=1;
 Instance.HeightMap.Size.z:=SizeZ;
 Instance.HeightMap.CellSize:=Vector3Origin;
 if (Instance.HeightMap.Width>0) and (Instance.HeightMap.Height>0) then begin
  mi:=PhysicsObjectAddMesh(Instance);
  Mesh:=Instance.Meshs^[mi];
  i:=Width*Height*sizeof(TPhysicsFloat);
  getmem(Instance.HeightMap.Data,i);
  move(Data^,Instance.HeightMap.Data^,i);
  Instance.HeightMap.CellSize.x:=Instance.HeightMap.Size.x/Instance.HeightMap.Width;
  Instance.HeightMap.CellSize.y:=1;
  Instance.HeightMap.CellSize.z:=Instance.HeightMap.Size.z/Instance.HeightMap.Height;
  HalfSize.x:=Instance.HeightMap.Size.x*0.5;
  HalfSize.y:=0;
  HalfSize.z:=Instance.HeightMap.Size.z*0.5;
  for y:=0 to Instance.HeightMap.Height-1 do begin
   for x:=0 to Instance.HeightMap.Width-1 do begin
    Quad[0]:=Vector3Sub(Vector3(x*Instance.HeightMap.CellSize.x,PhysicsObjectHeightMapGetHeight(Instance,x,y),y*Instance.HeightMap.CellSize.z),HalfSize);
    Quad[1]:=Vector3Sub(Vector3(x*Instance.HeightMap.CellSize.x,PhysicsObjectHeightMapGetHeight(Instance,x,y+1),(y+1)*Instance.HeightMap.CellSize.z),HalfSize);
    Quad[2]:=Vector3Sub(Vector3((x+1)*Instance.HeightMap.CellSize.x,PhysicsObjectHeightMapGetHeight(Instance,x+1,y+1),(y+1)*Instance.HeightMap.CellSize.z),HalfSize);
    Quad[3]:=Vector3Sub(Vector3((x+1)*Instance.HeightMap.CellSize.x,PhysicsObjectHeightMapGetHeight(Instance,x+1,y),y*Instance.HeightMap.CellSize.z),HalfSize);
    PhysicsObjectMeshAddQuad(Mesh^,@Quad);
   end;
  end;
  PhysicsObjectMeshSubdivide(Mesh^);
  PhysicsObjectMeshFinish(Mesh^);
 end;
end;

procedure PhysicsObjectUpdateTransform(var Instance:TPhysicsObject); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Instance.TransformAABB:=AABBTransform(Instance.AABB,Instance.Transform);
 Instance.TransformSphere.Center:=Vector3Add(Instance.Sphere.Center,Instance.Position);
 Instance.HaveNewTransform:=true;
end;

procedure PhysicsObjectUpdate(var Instance:TPhysicsObject;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
end;

procedure PhysicsObjectUpdatePosition(var Instance:TPhysicsObject;Position:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Instance.OldPosition:=Instance.Position;
 Instance.Position:=Position;
 PhysicsObjectUpdateTransform(Instance);
end;

procedure PhysicsObjectSetRigidBody(var Instance:TPhysicsObject;RigidBody:PPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Instance.IsIdentity:=false;
 Instance.RigidBody:=RigidBody;
end;

procedure PhysicsObjectSetVector(var Instance:TPhysicsObject;v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  IsIdentity:=false;
  OldTransform:=Transform;
  OldInvTransform:=InvTransform;
  Matrix4x4Translate(Transform,v);
  Matrix4x4Inverse(InvTransform,Transform);
  PhysicsObjectUpdatePosition(Instance,v);
  if assigned(RigidBody) then begin
   PhysicsRigidBodySetVector(RigidBody^,v);
  end;
 end;
 PhysicsObjectUpdateTransform(Instance);
end;

procedure PhysicsObjectSetMatrix(var Instance:TPhysicsObject;m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var v:TPhysicsVector3;
begin
 with Instance do begin
  IsIdentity:=false;
  OldTransform:=Transform;
  OldInvTransform:=InvTransform;
  Transform:=m;
  Matrix4x4Inverse(InvTransform,Transform);
  v:=Vector3Origin;
  Vector3MatrixMul(v,m);
  PhysicsObjectUpdatePosition(Instance,v);
  if assigned(RigidBody) then begin
   PhysicsRigidBodySetMatrix(RigidBody^,m);
  end;
 end;
 PhysicsObjectUpdateTransform(Instance);
end;

function PhysicsObjectGetMin(var Instance:TPhysicsObject):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.AABB.Min;
end;

function PhysicsObjectGetMax(var Instance:TPhysicsObject):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.AABB.Max;
end;

function PhysicsObjectGetCenter(var Instance:TPhysicsObject):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.Sphere.Center;
end;

function PhysicsObjectGetRadius(var Instance:TPhysicsObject):TPhysicsFloat; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.Sphere.Radius;
end;

function PhysicsObjectMeshRayIntersectionMesh(var ObjectInstance:TPhysicsObject;var Instance:TPhysicsObjectMesh;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3;var nearest:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    org,dir,p:TPhysicsVector3;
    dot:TPhysicsFloat;
    t:PPhysicsTriangle;
begin
//PhysicsObjectTriangleTreeTraverse(Instance,Line0,Line1);
 result:=false;
{for i:=0 to Instance.NumMeshs-1 do begin
  result:=PhysicsObjectMeshRayIntersectionMesh(ObjectInstance,Instance.Meshs^[i]^,Origin,Direction,Point,Normal,Nearest);
 end;}
 org:=Vector3TermMatrixMul(Origin,ObjectInstance.InvTransform);
{dir:=Vector3TermMatrixMul(Vector3Add(Origin,Direction),Instance.InvTransform);
 dir:=Vector3ScalarMul(Vector3Sub(dir,org),INFINITY);
 dir:=Vector3Norm(Vector3TermMatrixMul(Direction,Matrix4x4Rotation(Instance.InvTransform)));}
 dir:=Vector3ScalarMul(Vector3Norm(Vector3TermMatrixMul(Direction,Matrix4x4Rotation(ObjectInstance.InvTransform))),INFINITY);
 dot:=Vector3Dot(dir,dir);
 if abs(dot)<EPSILON then exit;
 dot:=(Vector3Dot(dir,Instance.Sphere.Center)-Vector3Dot(dir,org))/dot;
 if (dot<0) and (Vector3Dist(Instance.Sphere.Center,org)>Instance.Sphere.Radius) then exit;
 if (dot>1) and (Vector3Dist(Instance.Sphere.Center,Vector3Add(org,dir))>Instance.Sphere.Radius) then exit;
 for i:=0 to Instance.NumMeshs-1 do begin
  {result:=}PhysicsObjectMeshRayIntersectionMesh(ObjectInstance,Instance.Meshs^[i]^,Origin,Direction,Point,Normal,Nearest);
 end;
 for i:=0 to Instance.NumTriangles-1 do begin
  t:=@Instance.Triangles^[i];{}
{for i:=0 to Instance.NumTriangleTreeBufferTriangles-1 do begin
  t:=Instance.TriangleTreeBuffer^[i];{}
  dot:=Vector3Dot(t^.Plane.Normal,dir);
  if abs(dot)<EPSILON then continue;
  dot:=-(PlaneVectorDistance(t^.plane,Vector4(org,1))/dot);
  if (dot<0) or (dot>1) then continue;
  p:=Vector3Add(org,Vector3ScalarMul(dir,dot));
  if (dot<nearest) and (PlaneVectorDistance(t^.FastTriangleCheckPlanes[0],p)>0) and
                       (PlaneVectorDistance(t^.FastTriangleCheckPlanes[1],p)>0) and
                       (PlaneVectorDistance(t^.FastTriangleCheckPlanes[2],p)>0) then begin
   nearest:=dot;
   Point:=Vector3TermMatrixMul(p,ObjectInstance.Transform);
   Normal:=Vector3TermMatrixMul(t^.Plane.Normal,Matrix4x4Rotation(ObjectInstance.Transform));
  end;
 end;
 result:=nearest<=2;
end;

function PhysicsObjectRayIntersectionMesh(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var nearest:TPhysicsFloat;
    i:integer;
begin
 nearest:=2.5;
 case Instance.CollisionBodyType of
  BodyBox,BodyCylinder:begin
   result:=PhysicsObjectMeshRayIntersectionMesh(Instance,Instance.BoxMesh,Origin,Direction,Point,Normal,nearest);
  end;
  BodyConvexHull:begin
   result:=PhysicsObjectMeshRayIntersectionMesh(Instance,Instance.ConvexHullMesh,Origin,Direction,Point,Normal,nearest);
  end;
  else begin
   result:=false;
   for i:=0 to Instance.NumMeshs-1 do begin
    if PhysicsObjectMeshRayIntersectionMesh(Instance,Instance.Meshs^[i]^,Origin,Direction,Point,Normal,nearest) then begin
     result:=true;
    end;
   end;
  end;
 end;
end;

function PhysicsObjectRayIntersectionBox(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var s,v,h:TPhysicsVector3;
    lo,hi,k,Alpha:TPhysicsFloat;
    nlo,nhi,n:integer;
    sign:array[0..2] of TPhysicsFloat;
begin
 result:=false;
 s:=Vector3TermMatrixMul(Vector3Sub(Origin,Instance.Position),Matrix4x4Rotation(Instance.Transform));
 v:=Vector3TermMatrixMul(Direction,Matrix4x4Rotation(Instance.Transform));
 if v.x<0 then begin
  s.x:=-s.x;
  v.x:=-v.x;
  sign[0]:=1;
 end else begin
  sign[0]:=-1;
 end;
 if v.y<0 then begin
  s.y:=-s.y;
  v.y:=-v.y;
  sign[1]:=1;
 end else begin
  sign[1]:=-1;
 end;
 if v.z<0 then begin
  s.z:=-s.z;
  v.z:=-v.z;
  sign[2]:=1;
 end else begin
  sign[2]:=-1;
 end;
 h:=Vector3ScalarMul(Vector3Sub(Instance.AABB.Max,Instance.AABB.Min),0.5);
 if (((s.x<-h.x) and (v.x<=0)) or (s.x>h.x)) or
    (((s.y<-h.y) and (v.y<=0)) or (s.y>h.y)) or
    (((s.z<-h.z) and (v.z<=0)) or (s.z>h.z)) then begin
  exit;
 end;
 lo:=-INFINITY;
 hi:=INFINITY;
 nlo:=0;
 nhi:=0;
 if abs(v.x)>EPSILON then begin
  k:=(-h.x-s.x)/v.x;
  if k>lo then begin
   lo:=k;
   nlo:=0;
  end;
  k:=(h.x-s.x)/v.x;
  if k<hi then begin
   hi:=k;
   nhi:=0;
  end;
 end;
 if abs(v.y)>EPSILON then begin
  k:=(-h.y-s.y)/v.y;
  if k>lo then begin
   lo:=k;
   nlo:=1;
  end;
  k:=(h.y-s.y)/v.y;
  if k<hi then begin
   hi:=k;
   nhi:=1;
  end;
 end;
 if abs(v.z)>EPSILON then begin
  k:=(-h.z-s.z)/v.z;
  if k>lo then begin
   lo:=k;
   nlo:=2;
  end;
  k:=(h.z-s.z)/v.z;
  if k<hi then begin
   hi:=k;
   nhi:=2;
  end;
 end;
 if lo>hi then exit;
 if lo>=0 then begin
  Alpha:=lo;
  n:=nlo;
 end else begin
  Alpha:=hi;
  n:=nhi;
 end;
 if Alpha<0 then exit;
 Point:=Vector3Add(Origin,Vector3ScalarMul(Direction,Alpha));
 Normal:=Vector3ScalarMul(Vector3(Instance.Transform[n,0],Instance.Transform[n,1],Instance.Transform[n,2]),sign[n]);
 result:=true;
end;

function PhysicsObjectRayIntersectionSphere(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var org,dir,m:TPhysicsVector3;
    p,d,s1,s2,dist:TPhysicsFloat;
begin
 result:=false;
 org:=Vector3TermMatrixMul(Origin,Instance.InvTransform);
 dir:=Vector3Norm(Vector3TermMatrixMul(Direction,Matrix4x4Rotation(Instance.InvTransform)));
 m:=Vector3Sub(org,Instance.Sphere.Center);
 p:=-Vector3Dot(m,dir);
 d:=sqr(p)-Vector3LengthSquared(m)+sqr(Instance.Sphere.Radius);
 if d<=0 then exit;
 d:=sqrt(d);
 s1:=p-d;
 s2:=p+d;
 if S2>0 then begin
  if S1<0 then begin
   dist:=S2;
  end else begin
   dist:=S1;
  end;
 end else begin
  exit;
 end;
 Point:=Vector3TermMatrixMul(Vector3Add(org,Vector3ScalarMul(dir,dist)),Instance.Transform);
 Normal:=Vector3Norm(Vector3Sub(Point,Vector3TermMatrixMul(Instance.Sphere.Center,Instance.Transform)));
 result:=true;
end;

function PhysicsObjectRayIntersectionCylinder(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Radius,HalfLength,d,a,b,c,k,uv,sign,internal,Alpha:TPhysicsFloat;
    GeometryDirection,q,r:TPhysicsVector3;
begin
 // TO-DO: BUG FIX IT! (ending caps)
 result:=false;
 Radius:=Max(abs(Instance.AABB.Max.x-Instance.AABB.Min.x),abs(Instance.AABB.Max.y-Instance.AABB.Min.y))*0.5;
 HalfLength:=abs(Instance.AABB.Max.z-Instance.AABB.Min.z)*0.5;
 r:=Vector3Sub(Origin,Instance.Position);
//GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(WithObject^.Transform));
 GeometryDirection:=Vector3(Instance.Transform[2,0],Instance.Transform[2,1],Instance.Transform[2,2]);
 d:=Vector3Dot(GeometryDirection,r);
 q:=Vector3Sub(Vector3ScalarMul(GeometryDirection,d),r);
 c:=Vector3Dot(q,q)-sqr(Radius);
 uv:=Vector3Dot(GeometryDirection,Direction);
 r:=Vector3Sub(Vector3ScalarMul(GeometryDirection,uv),Direction);
 a:=Vector3Dot(r,r);
 b:=2*Vector3Dot(q,r);
 k:=sqr(b)-(4*a*c);
 if (k<EPSILON) and (c<=0) then begin
  if uv<0 then begin
   sign:=-1;
  end else begin
   sign:=1;
  end;
  if (d>=-HalfLength) and (d<=HalfLength) then begin
   internal:=-1;
  end else begin
   internal:=1;
  end;
  if (((uv>0) and (d+(sign*INFINITY)<HalfLength*internal)) or ((uv<0) and (d+(sign*INFINITY)>HalfLength*internal))) then exit;
  Point:=Vector3Add(Origin,Vector3ScalarMul(Direction,(-sign*d)-(internal*HalfLength)));
  Normal:=Vector3ScalarMul(GeometryDirection,sign);
  result:=true;
  exit;
 end;
 if k>EPSILON then begin
  k:=sqrt(k);
  a:=1/(2*a);
  Alpha:=(-b-k)*a;
  if Alpha<0 then begin
   Alpha:=(-b+k)*a;
   if Alpha<0 then begin
    exit;
   end;
  end;
  Point:=Vector3Add(Origin,Vector3ScalarMul(Direction,Alpha));
  q:=Vector3Sub(Point,Instance.Position);
  d:=Vector3Dot(q,GeometryDirection);
  if (d>=-HalfLength) and (d<=HalfLength) then begin
   if c<0 then begin
    sign:=-1;
   end else begin
    sign:=1;
   end;
   Normal:=Vector3Norm(Vector3ScalarMul(Vector3Sub(Point,Vector3Add(Instance.Position,Vector3ScalarMul(GeometryDirection,d))),sign));
   result:=true;
  end;
 end;
end;

function PhysicsObjectRayIntersectionCapsule(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var GeometryDirection,cs,q,r,rq:TPhysicsVector3;
    Radius,HalfLength,k,c,uv,a,b,Alpha,sign:TPhysicsFloat;
    inside:boolean;
begin
 result:=false;
 Radius:=Max(abs(Instance.AABB.Max.x-Instance.AABB.Min.x),abs(Instance.AABB.Max.y-Instance.AABB.Min.y))*0.5;
 HalfLength:=(abs(Instance.AABB.Max.z-Instance.AABB.Min.z)-(radius*2))*0.5;
 cs:=Vector3Sub(Origin,Instance.Position);
//GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(WithObject^.Transform));
 GeometryDirection:=Vector3(Instance.Transform[2,0],Instance.Transform[2,1],Instance.Transform[2,2]);
 k:=Vector3Dot(GeometryDirection,cs);
 q:=Vector3Sub(Vector3ScalarMul(GeometryDirection,k),cs);
 c:=Vector3Dot(q,q)-sqr(radius);
 inside:=false;
 if c<0 then begin
  if k<-HalfLength then begin
   k:=-HalfLength;
  end else if k>HalfLength then begin
   k:=HalfLength;
  end;
  r:=Vector3Add(Instance.Position,Vector3ScalarMul(GeometryDirection,k));
  if Vector3LengthSquared(Vector3Sub(Origin,r))<sqr(radius) then begin
   inside:=true;
  end;
 end;
 if (c<0) and not inside then begin
  if k<0 then begin
   k:=-HalfLength;
  end else begin
   k:=HalfLength;
  end;
 end else begin
  uv:=Vector3Dot(GeometryDirection,Direction);
  r:=Vector3Sub(Vector3ScalarMul(GeometryDirection,uv),Direction);
  a:=Vector3Dot(r,r);
  b:=2*Vector3Dot(q,r);
  k:=sqr(b)-(4*a*c);
  if k<0 then begin
   if not inside then exit;
   if uv<0 then begin
    k:=-HalfLength;
   end else begin
    k:=HalfLength;
   end;
  end else begin
   k:=sqrt(k);
   a:=1/(2*a);
   Alpha:=(-b-k)*a;
   if Alpha<0 then begin
    Alpha:=(-b+k)*a;
    if Alpha<0 then begin
     exit;
    end;
   end;
   Point:=Vector3Add(Origin,Vector3ScalarMul(Direction,Alpha));
   q:=Vector3Sub(Point,Instance.Position);
   k:=Vector3Dot(q,GeometryDirection);
   if (k>=-HalfLength) and (k<=HalfLength) then begin
    if inside then begin
     sign:=-1;
    end else begin
     sign:=1;
    end;
    Normal:=Vector3Norm(Vector3ScalarMul(Vector3Sub(Point,Vector3Add(Instance.Position,Vector3ScalarMul(GeometryDirection,k))),Sign));
    result:=true;
    exit;
   end;
   if k<0 then begin
    k:=-HalfLength;
   end else begin
    k:=HalfLength;
   end;
  end;
 end;
 q:=Vector3Add(Instance.Position,Vector3ScalarMul(GeometryDirection,k));
 rq:=Vector3Sub(Origin,q);
 b:=Vector3Dot(rq,Direction);
 c:=Vector3Dot(rq,rq)-sqr(Radius);
 k:=sqr(b)-c;
 if k<=0 then exit;
 k:=sqrt(k);
 if inside and (c>=0) then begin
  Alpha:=-b+k;
  if Alpha<0 then exit;
 end else begin
  Alpha:=-b-k;
  if Alpha<0 then begin
   Alpha:=-b+k;
   if Alpha<0 then exit;
  end;
 end;
 Point:=Vector3Add(Origin,Vector3ScalarMul(Direction,Alpha));
 if inside or (c<0) then begin
  sign:=-1;
 end else begin
  sign:=1;
 end;
 Normal:=Vector3Norm(Vector3ScalarMul(Vector3Sub(Point,rq),sign));
 result:=true;
end;

function PhysicsObjectRayIntersectionPlane(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Alpha,sign,k:TPhysicsFloat;
begin
 result:=false;
 Alpha:=Instance.ObjectPlane.Distance-Vector3Dot(Instance.ObjectPlane.Normal,Origin);
 if Alpha>0 then begin
  sign:=-1;
 end else begin
  sign:=1;
 end;
 k:=Vector3Dot(Instance.ObjectPlane.Normal,Direction);
 if abs(k)<EPSILON then exit;
 Point:=Vector3Add(Origin,Vector3ScalarMul(Direction,Alpha));
 Normal:=Vector3ScalarMul(Instance.ObjectPlane.Normal,sign);
 result:=true;
end;

const ObjectLineIntersectionProcs:TPhysicsObjectRayIntersectionProcs=(PhysicsObjectRayIntersectionMesh,
                                                                      PhysicsObjectRayIntersectionBox,
                                                                      PhysicsObjectRayIntersectionSphere,
                                                                      PhysicsObjectRayIntersectionCylinder,
                                                                      PhysicsObjectRayIntersectionCapsule,
                                                                      PhysicsObjectRayIntersectionPlane,
                                                                      PhysicsObjectRayIntersectionMesh,
                                                                      PhysicsObjectRayIntersectionMesh);

function PhysicsObjectRayIntersection(var Instance:TPhysicsObject;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3;UserRayProc:TPhysicsObjectRayIntersectionUserProc=nil;UserData:pointer=nil):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if not SphereRayIntersect(Instance.TransformSphere,Origin,Direction) then begin
  result:=false;
  exit;
 end;
 if assigned(UserRayProc) and UserRayProc(Instance,Origin,Direction,Point,Normal,UserData) then begin
  result:=true;
  exit;
 end;
 result:=ObjectLineIntersectionProcs[Instance.CollisionBodyType](Instance,Origin,Direction,Point,Normal);
end;

procedure PhysicsCollideInit(var Instance:TPhysicsCollide); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 fillchar(Instance,sizeof(TPhysicsCollide),#0);
end;

procedure PhysicsCollideDone(var Instance:TPhysicsCollide); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
begin
   with Instance do begin
    for i:=0 to AllContacts-1 do begin
     if assigned(Contacts^[i]) then begin
      dispose(Contacts^[i]);
      Contacts^[i]:=nil;
     end;
    end;
    if assigned(Contacts) then begin
     freemem(Contacts);
     Contacts:=nil;
    end;
    if assigned(Objects) then begin
     freemem(Objects);
     Objects:=nil;
    end;
   end;
   fillchar(Instance,sizeof(TPhysicsCollide),#0);
end;

function PhysicsCollideAddContact(var Instance:TPhysicsCollide;TheObject:PPhysicsObject;const ThePoint,TheNormal:TPhysicsVector3;TheDepth:TPhysicsFloat;SwapObjects:boolean=false;TheMinDepth:boolean=false;DoTransform:boolean=true):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var p,n:TPhysicsVector3;
    i,OldContacts:integer;
    c:PPhysicsContact;
begin
 with Instance do begin
  if NumContacts>=CollideMaxContacts then begin
   result:=false;
   exit;
  end;
  p:=ThePoint;
  n:=TheNormal;
  if DoTransform and assigned(TheObject) and not TheObject^.IsIdentity then begin
   Vector3MatrixMul(p,TheObject^.Transform);
   Vector3MatrixMul(n,Matrix4x4Rotation(TheObject^.Transform));
  end;
  if SwapObjects then begin
   n:=Vector3Neg(n);
  end;
  inc(NumContacts);
  if NumContacts>AllContacts then begin
   OldContacts:=AllContacts;
   AllContacts:=(NumContacts+MemoryInc) and not MemoryIncMask;
   PhysicsReallocateMemory(Contacts,AllContacts*sizeof(TPhysicsContact));
   for i:=OldContacts to AllContacts do begin
    new(Contacts^[i]);
   end;
  end;
  c:=Contacts^[NumContacts-1];
  fillchar(c^,sizeof(TPhysicsContact),#0);
  if TheMinDepth then begin
   for i:=0 to NumContacts-1 do begin
    if Vector3Compare(Contacts^[i]^.Point,p) then begin
     dec(NumContacts);
     if Contacts^[i].Depth<TheDepth then begin
      result:=true;
      exit;
     end;
     c:=Contacts^[i];
     break;
    end;
   end;
  end;

  if assigned(TheObject) then begin
   if NumObjects=0 then begin
{   if NumObjects>=CollideMaxObjects then begin
     result:=false;
     exit;
    end;}
    inc(NumObjects);
    if NumObjects>AllObjects then begin
     AllObjects:=(NumObjects+MemoryInc) and not MemoryIncMask;
     PhysicsReallocateMemory(Objects,AllObjects*sizeof(PPhysicsObject));
    end;
    Objects[NumObjects-1]:=TheObject;
   end else begin
    for i:=0 to NumObjects-1 do begin
     if Objects[NumObjects-1]=TheObject then begin
      break;
     end else if i=(NumObjects-1) then begin
      inc(NumObjects);
      if NumObjects>AllObjects then begin
       AllObjects:=(NumObjects+MemoryInc) and not MemoryIncMask;
       PhysicsReallocateMemory(Objects,AllObjects*sizeof(PPhysicsObject));
      end;
      Objects[NumObjects-1]:=TheObject;
      break;
     end;
    end;
   end;
   c^.ContactObject:=TheObject;
  end else begin
   c^.ContactObject:=nil;
  end;
  c^.Point:=p;
  c^.Normal:=n;
  c^.Depth:=TheDepth;

{$ifdef physicsshowcontacts}
  glDepthFunc(GL_ALWAYS);
  glPointSize(5);
  glLineWidth(5);
  glColor4f(1,1,1,1);
  glBegin(GL_POINTS);
{$ifdef physicsdouble}
  glVertex3dv(@c^.Point);
{$else}
  glVertex3fv(@c^.Point);
{$endif}
  glEnd();
  glColor4f(0.25,1,0.25,1);
  glBegin(GL_LINES);
{$ifdef physicsdouble}
  glVertex3dv(@c^.Point);
{$else}
  glVertex3fv(@c^.Point);
{$endif}
  p:=Vector3Add(c^.Point,Vector3ScalarMul(c^.Normal,c^.Depth+1));
{$ifdef physicsdouble}
  glVertex3dv(@p);
{$else}
  glVertex3fv(@p;
{$endif}
  glEnd();
  glLineWidth(1);
{$endif}

  result:=true;
 end;
end;

const NextTriangle:array[0..2] of integer=(1,2,0);
      NextAxis:array[0..2] of integer=(1,2,0);

procedure PhysicsCollidePointMesh(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ThePosition:TPhysicsVector3;
 procedure CheckTriangle(Triangle:PPhysicsTriangle);
 var Dist,Depth,d,Dot:TPhysicsFloat;
     Normal,Point,Edge,Dir,p:TPhysicsVector3;
     i:integer;
 begin
  if not assigned(Triangle) then exit;

  Dist:=PlaneVectorDistance(Triangle^.Plane,ThePosition);
  if (Dist<0) or (Dist>=Radius) then exit;

  Normal:=Triangle^.Plane.Normal;
  Point:=Vector3Sub(ThePosition,Vector3ScalarMul(Normal,Radius));
  Depth:=Radius-Dist;

  i:=0;
  while i<3 do begin
   if PlaneVectorDistance(Triangle^.FastTriangleCheckPlanes[i],Point)<0 then break;
   inc(i);
  end;

  if i<>3 then begin
   Point:=Vector3Sub(ThePosition,Vector3ScalarMul(Normal,Dist));
   i:=0;
   while i<3 do begin
    Edge:=Vector3Sub(Triangle^.Vertices[NextTriangle[i]],Triangle^.Vertices[i]);
    Dir:=Vector3Norm(Vector3Cross(Edge,Normal));

    d:=Vector3Dot(Vector3Sub(Point,Triangle^.Vertices[i]),Dir);
    if (d>=Radius) or (d<=0) then begin
     inc(i);
     continue;
    end;

    p:=Vector3Sub(Point,Vector3ScalarMul(Dir,d));

    Dot:=Vector3Dot(p,Edge);
    if Dot>Vector3Dot(Triangle^.Vertices[NextTriangle[i]],Edge) then begin
     p:=Triangle^.Vertices[NextTriangle[i]];
    end else if Dot<Vector3Dot(Triangle^.Vertices[i],Edge) then begin
     p:=Triangle^.Vertices[i];
    end;

    d:=Vector3Length(Vector3Sub(Point,p));
    if d>Radius then begin
     inc(i);
     continue;
    end;

    Depth:=sqrt(sqr(Radius)-sqr(d))-Dist;
    if Depth<=0 then begin
     inc(i);
     continue;
    end;

    Point:=Vector3Sub(p,Vector3ScalarMul(Normal,Depth));

    break;
   end;
   if i=3 then exit;
  end;

  if not PhysicsCollideAddContact(Instance,WithObject,Point,Normal,Depth) then exit;
 end;
 procedure CheckMesh(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  if Vector3Length(Vector3Sub(Mesh.Sphere.Center,ThePosition))>(Mesh.Sphere.Radius+Radius) then exit;
  for i:=0 to Mesh.NumMeshs-1 do begin
   CheckMesh(Mesh.Meshs^[i]^);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   CheckTriangle(@Mesh.Triangles[i]);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
 end;
var i:integer;
begin
 if not assigned(WithObject) then exit;

 ThePosition:=Point;
 Vector3MatrixMul(ThePosition,WithObject^.InvTransform);
 if Vector3Length(Vector3Sub(WithObject^.Sphere.Center,ThePosition))>(WithObject^.Sphere.Radius+Radius) then exit;

 case WithObject^.CollisionBodyType of
  BodyBox,BodyCylinder:begin
   CheckMesh(WithObject^.BoxMesh);
  end;
  BodyConvexHull:begin
   CheckMesh(WithObject^.ConvexHullMesh);
  end;
  else begin
   for i:=0 to WithObject^.NumMeshs-1 do begin
    CheckMesh(WithObject^.Meshs^[i]^);
    if Instance.NumContacts>=CollideMaxContacts then exit;
   end;
  end;
 end;
end;

procedure PhysicsCollidePointBox(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ClosestBoxPoint,Normal:TPhysicsVector3;
    Dist,Depth:TPhysicsFloat;
begin
 Dist:=sqrt(Max(BoxGetDistanceToPoint(Point,Vector3Origin,Vector3Sub(WithObject^.AABB.Max,WithObject^.AABB.Min),WithObject^.InvTransform,WithObject^.Transform,ClosestBoxPoint),EPSILON));
 Depth:=Radius-Dist;
 if Depth>=0 then begin
  if Dist<-EPSILON then begin
   Normal:=Vector3Norm(Vector3Sub(Vector3Sub(ClosestBoxPoint,Point),ClosestBoxPoint));
  end else if Dist>EPSILON then begin
   Normal:=Vector3Norm(Vector3Sub(Point,ClosestBoxPoint));
  end else begin
   Normal:=Vector3Norm(Vector3Sub(Point,WithObject^.Position));
  end;
  if not PhysicsCollideAddContact(Instance,WithObject,ClosestBoxPoint,Normal,Depth,false,false,false) then exit;
 end;
end;

procedure PhysicsCollidePointSphere(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var r1,r2:TPhysicsFloat;
    Position,Normal:TPhysicsVector3;
begin
 r1:=Radius;
 r2:=WithObject^.Sphere.Radius;
 Position:=Vector3Sub(Point,WithObject^.Position);
 if Vector3Length(Position)<(r1+r2) then begin
  Normal:=Vector3Norm(Position);
  if not PhysicsCollideAddContact(Instance,WithObject,Vector3ScalarMul(Normal,r1),Normal,r1+r2-Vector3Length(Position)) then exit;
 end;
end;

procedure PhysicsCollidePointCylinder(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var CylinderRotation:TPhysicsMatrix3x3;
    CylinderPosition,CylinderAxis,CylinderPointStart,CylinderPointEnd,Diff0,
    Diff1,Project,Dir,p1,p2,Pos,Normal:TPhysicsVector3;
    CylinderRadius,CylinderSize,k,Dist,
    Depth:TPhysicsFloat;
begin
 CylinderRotation:=Matrix4x4GetSubMatrix3x3(Matrix4x4Rotation(WithObject^.Transform),0,0);
 CylinderPosition:=WithObject^.Position;
 CylinderRadius:=Max(abs(WithObject^.AABB.Max.x-WithObject^.AABB.Min.x),abs(WithObject^.AABB.Max.y-WithObject^.AABB.Min.y))*0.5;
 CylinderSize:=abs(WithObject^.AABB.Max.z-WithObject^.AABB.Min.z)*0.5;
 CylinderAxis:=Vector3(WithObject^.Transform[2,0],WithObject^.Transform[2,1],WithObject^.Transform[2,2]);
 CylinderPointStart:=Vector3Sub(CylinderPosition,Vector3ScalarMul(CylinderAxis,CylinderSize));
 CylinderPointEnd:=Vector3Add(CylinderPosition,Vector3ScalarMul(CylinderAxis,CylinderSize));
 Diff0:=Vector3Sub(Point,CylinderPointStart);
 k:=Vector3Dot(Diff0,CylinderAxis);
 if (k<=(-(CylinderRadius+WithObject^.Sphere.Radius))) or (k>=((2.0*CylinderSize)+CylinderRadius+Radius)) then exit;
 if k>2.0*CylinderSize then begin
  Project:=CylinderPointEnd;
 end else if k<0 then begin
  Project:=CylinderPointStart;
 end else begin
  Project:=Vector3Add(CylinderPointStart,Vector3ScalarMul(CylinderAxis,k));
 end;
 Diff1:=Vector3Sub(Project,Point);
 Dist:=sqrt(Vector3Dot(Diff1,Diff1));
 if Dist>(CylinderRadius+Radius) then exit;
 Depth:=CylinderRadius+Radius-Dist;
 if abs(Dist)>EPSILON then begin
  Dir:=Vector3ScalarMul(Diff1,1/DIST);
 end else begin
  Dir:=Vector3(WithObject^.Transform[0,0],WithObject^.Transform[0,1],WithObject^.Transform[0,2]);
 end;
 p1:=Vector3Sub(Project,Vector3ScalarMul(Dir,CylinderRadius));
 p2:=Vector3Add(WithObject^.Position,Vector3ScalarMul(Dir,CylinderRadius));
 Pos:=p1;
 Normal:=Vector3Norm(Vector3Sub(p2,p1));
 if not PhysicsCollideAddContact(Instance,WithObject,Pos,Normal,Depth,true,false,false) then exit;
end;

procedure PhysicsCollidePointCapsule(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Alpha,HalfLength,r1,r2,d,d1:TPhysicsFloat;
    Position,Normal,GeometryDirection:TPhysicsVector3;
begin
 r1:=Max(abs(WithObject^.AABB.Max.x-WithObject^.AABB.Min.x),abs(WithObject^.AABB.Max.y-WithObject^.AABB.Min.y))*0.5;
 r2:=Radius;
//GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(WithObject^.Transform));
 GeometryDirection:=Vector3(WithObject^.Transform[2,0],WithObject^.Transform[2,1],WithObject^.Transform[2,2]);
 Alpha:=(GeometryDirection.x*(Point.x-WithObject^.Position.x))+
        (GeometryDirection.y*(Point.y-WithObject^.Position.y))+
        (GeometryDirection.z*(Point.z-WithObject^.Position.z));
 HalfLength:=(abs(WithObject^.AABB.Max.z-WithObject^.AABB.Min.z)-(r1*2))*0.5;
 if Alpha>HalfLength then begin
  Alpha:=HalfLength;
 end else if alpha<-HalfLength then begin
  Alpha:=-HalfLength;
 end;
 Position:=Vector3Add(WithObject^.Position,Vector3ScalarMul(GeometryDirection,Alpha));
 d:=Vector3Dist(Position,Point);
 if d>(r1+r2) then exit;
 if d<=0 then begin
  if not PhysicsCollideAddContact(Instance,WithObject,Position,Vector3(1,0,0),r1+r2,true,false,false) then exit;
 end else begin
  d1:=1/d;
  Normal:=Vector3ScalarMul(Vector3Sub(Position,Point),d1);
  if not PhysicsCollideAddContact(Instance,WithObject,Vector3Add(Position,Vector3ScalarMul(Normal,(r2-r1-d)*0.5)),Normal,r1+r2-d,true,false,false) then exit;
 end;
end;

procedure PhysicsCollidePointPlane(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Position,WorldPos:TPhysicsVector3;
    Dist,Depth:TPhysicsFloat;
begin
 Position:=Vector3TermMatrixMul(Point,WithObject^.InvTransform);
 Dist:=PlaneVectorDistance(WithObject^.ObjectPlane,Position);
 if Dist>Radius then exit;
 Depth:=Radius-Dist;
 WorldPos:=Vector3Sub(Point,Vector3ScalarMul(WithObject^.ObjectPlane.Normal,Radius));
 if not PhysicsCollideAddContact(Instance,WithObject,WorldPos,WithObject^.ObjectPlane.Normal,Depth,false,false,false) then exit;
end;

procedure PhysicsCollidePointHeightMap(var Instance:TPhysicsCollide;WithObject:PPhysicsObject;const Point:TPhysicsVector3;Radius:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Dist,Depth:TPhysicsFloat;
    Normal,WorldPos:TPhysicsVector3;
begin
 Dist:=PhysicsObjectHeightMapGetHeightNormalDistance(WithObject^,Vector3TermMatrixMul(Point,WithObject^.InvTransform),Normal);
 if Dist>Radius then exit;
 Normal:=Vector3Norm(Vector3TermMatrixMul(Normal,Matrix4x4Rotation(WithObject^.Transform)));
 Depth:=Radius-Dist;
 WorldPos:=Vector3Sub(Point,Vector3ScalarMul(Normal,Radius));
 if not PhysicsCollideAddContact(Instance,WithObject,WorldPos,Normal,Depth,false,false,false) then exit;
end;

const PhysicsCollidePointProcs:TPhysicsCollidePointProcs=(PhysicsCollidePointMesh,      // BodyMesh
                                                          PhysicsCollidePointBox,       // BodyBox
                                                          PhysicsCollidePointSphere,    // BodySphere
                                                          PhysicsCollidePointCylinder,  // BodyCylinder
                                                          PhysicsCollidePointCapsule,   // BodyCapsule
                                                          PhysicsCollidePointPlane,     // BodyPlane
                                                          PhysicsCollidePointHeightMap, // BodyHeightMap
                                                          PhysicsCollidePointMesh);     // BodyConvexHull

procedure PhysicsCollideFinishObjectMesh(var Instance:TPhysicsCollide;var TheObjectMesh:TPhysicsObjectMesh;Transform:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Rotation:TPhysicsMatrix4x4;
    i:integer;
 procedure ProcessTriangle(t:PPhysicsTriangle);
 var N4:TPhysicsVector4;
     Normal:TPhysicsVector3;
 begin
  t^.TransformedVertices:=t^.Vertices;
  t^.FastTriangleCheckPlanes:=t^.FastTriangleCheckPlanes;
  t^.TransformedPlane:=t^.Plane;
  Vector3MatrixMul(t^.TransformedVertices[0],Transform);
  Vector3MatrixMul(t^.TransformedVertices[1],Transform);
  Vector3MatrixMul(t^.TransformedVertices[2],Transform);

  Normal:=t^.TransformedPlane.Normal;
  Vector3MatrixMul(Normal,Rotation);
  t^.TransformedPlane:=Plane(Normal,-Vector3Dot(Normal,t^.TransformedVertices[0]));

  N4:=t^.TransformedFastTriangleCheckPlanes[0].xyzw;
  Vector4MatrixMul(N4,Rotation);
  Normal:=Vector3(N4);
  t^.TransformedFastTriangleCheckPlanes[0]:=Plane(Normal,-Vector3Dot(Normal,t^.TransformedVertices[0]));

  N4:=t^.TransformedFastTriangleCheckPlanes[1].xyzw;
  Vector4MatrixMul(N4,Rotation);
  Normal:=Vector3(N4);
  t^.TransformedFastTriangleCheckPlanes[1]:=Plane(Normal,-Vector3Dot(Normal,t^.TransformedVertices[1]));

  N4:=t^.TransformedFastTriangleCheckPlanes[2].xyzw;
  Vector4MatrixMul(N4,Rotation);
  Normal:=Vector3(N4);
  t^.TransformedFastTriangleCheckPlanes[2]:=Plane(Normal,-Vector3Dot(Normal,t^.TransformedVertices[2]));
 end;
begin
 with Instance do begin
  Rotation:=Matrix4x4Rotation(Transform);
  for i:=0 to TheObjectMesh.NumTriangles-1 do begin
   ProcessTriangle(@TheObjectMesh.Triangles[i]);
  end;
 end;
end;

procedure PhysicsCollideFinishObject(var Instance:TPhysicsCollide;TheObject:PPhysicsObject;Transform:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
begin
 with Instance do begin
  for i:=0 to TheObject^.NumMeshs-1 do begin
   PhysicsCollideFinishObjectMesh(Instance,TheObject^.Meshs^[i]^,Transform);
  end;
 end;
end;

procedure PhysicsCollideObjectMeshMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var TheMatrix:TPhysicsMatrix4x4;
 procedure CheckTriangle(SrcTriangle,DstTriangle:PPhysicsTriangle);
 var i,j,CollideCount:integer;
     Dist:array[0..2] of TPhysicsFloat;
     v0,v1,Edge,p:TPhysicsVector3;
     PointInside,V0Inside,V1Inside:boolean;
     l,Distance,d:TPhysicsFloat;
 begin
  CollideCount:=0;
  for i:=0 to 2 do begin
   Dist[i]:=PlaneVectorDistance(DstTriangle^.Plane,SrcTriangle^.TransformedVertices[i]);
   if abs(Dist[i])>(TheObject^.Sphere.Radius*2) then begin
    CollideCount:=3;
    break;
   end;
   if Dist[i]>0 then begin
    inc(CollideCount);
   end else if Dist[i]<0 then begin
    dec(CollideCount);
   end;
  end;
  if abs(CollideCount)=3 then exit;

  for i:=0 to 2 do begin
   if not ((Dist[i]<=0) xor (Dist[NextTriangle[i]]<=0)) then continue;

   v0:=SrcTriangle^.TransformedVertices[i];
   v1:=SrcTriangle^.TransformedVertices[NextTriangle[i]];
   Edge:=Vector3Sub(v1,v0);
   p:=Vector3Sub(v0,Vector3ScalarMul(Edge,Dist[i]/(Dist[NextTriangle[i]]-Dist[i])));

   PointInside:=(PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[0],p)>0) and
                (PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[1],p)>0) and
                (PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[2],p)>0);
   if not PointInside then continue;

   V0Inside:=(PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[0],v0)>0) and
             (PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[1],v0)>0) and
             (PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[2],v0)>0);

   V1Inside:=(PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[0],v1)>0) and
             (PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[1],v1)>0) and
             (PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[2],v1)>0);

   if V0Inside and V1Inside then begin
    l:=Vector3Length(Edge)*0.5;
    if (Dist[i]<0) and (Dist[i]>(-l)) then begin
     if not PhysicsCollideAddContact(Instance,ContactObject,v0,DstTriangle^.Plane.Normal,-Dist[i],SwapObjects,true) then exit;
    end else if (Dist[NextTriangle[i]]<0) and (Dist[NextTriangle[i]]>(-l)) then begin
     if not PhysicsCollideAddContact(Instance,ContactObject,v1,DstTriangle^.Plane.Normal,-Dist[NextTriangle[i]],SwapObjects,true) then exit;
    end;
   end else if V0Inside<>V1Inside then begin
    Distance:=0;
    for j:=0 to 2 do begin
     d:=PlaneVectorDistance(DstTriangle^.FastTriangleCheckPlanes[j],p);
     if j=0 then begin
      Distance:=d;
     end else if Distance>d then begin
      Distance:=d;
     end;
    end;
    if Dist[i]<0 then begin
     d:=Vector3Length(Vector3Sub(v0,p));
     if Distance>d then begin
      Distance:=0;
     end else begin
      Distance:=-(Dist[i]*(Distance/d));
     end;
    end else begin
     d:=Vector3Length(Vector3Sub(v1,p));
     if Distance>d then begin
      Distance:=0;
     end else begin
      Distance:=-(Dist[NextTriangle[i]]*(Distance/d));
     end;
    end;
    if not PhysicsCollideAddContact(Instance,ContactObject,p,Vector3Neg(SrcTriangle^.TransformedPlane.Normal),Distance,SwapObjects) then exit;
   end;
  end;

  if (assigned(WithObject^.RigidBody) and WithObject^.RigidBody^.Static) or not assigned(WithObject^.RigidBody) then begin
   CollideCount:=0;
   for i:=0 to 2 do begin
    Dist[i]:=PlaneVectorDistance(SrcTriangle^.TransformedPlane,DstTriangle^.Vertices[i]);
    if Dist[i]>0 then begin
     inc(CollideCount);
    end else if Dist[i]<0 then begin
     dec(CollideCount);
    end;
   end;
   if abs(CollideCount)=3 then exit;

   for i:=0 to 2 do begin
    if (Dist[i]>=0) or (Dist[NextTriangle[i]]<=0) then continue;

    v0:=DstTriangle^.Vertices[i];
    v1:=DstTriangle^.Vertices[NextTriangle[i]];
    Edge:=Vector3Sub(v1,v0);
    p:=Vector3Sub(v0,Vector3ScalarMul(Edge,Dist[i]/(Dist[NextTriangle[i]]-Dist[i])));

    PointInside:=(PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[0],p)>0) and
                 (PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[1],p)>0) and
                 (PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[2],p)>0);
    if not PointInside then continue;

    V0Inside:=(PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[0],v0)>0) and
              (PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[1],v0)>0) and
              (PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[2],v0)>0);

    V1Inside:=(PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[0],v1)>0) and
              (PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[1],v1)>0) and
              (PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[2],v1)>0);

    if V0Inside<>V1Inside then begin
     Distance:=0;
     for j:=0 to 2 do begin
      d:=PlaneVectorDistance(SrcTriangle^.TransformedFastTriangleCheckPlanes[j],p);
      if j=0 then begin
       Distance:=d;
      end else if Distance>d then begin
       Distance:=d;
      end;
     end;

     d:=Vector3Length(Vector3Sub(v0,p));
     if Distance>d then begin
      Distance:=0;
     end else begin
      Distance:=-(Dist[i]*(Distance/d));
     end;

     if not PhysicsCollideAddContact(Instance,ContactObject,p,DstTriangle^.Plane.Normal,Distance,SwapObjects) then exit;
    end;
   end;
  end;
 end;
 procedure CheckWithMesh(var TheMesh,WithMesh:TPhysicsObjectMesh;var Finished:boolean);
 var i,j:integer;
 begin
  if Vector3Length(Vector3Sub(Vector3Add(TheMesh.Sphere.Center,TheObject^.Position),Vector3Add(WithMesh.Sphere.Center,WithObject^.Position)))>(TheMesh.Sphere.Radius+WithMesh.Sphere.Radius) then begin
   exit;
  end;
  if not Finished then begin
   PhysicsCollideFinishObjectMesh(Instance,TheMesh,TheMatrix);
   Finished:=true;
  end;
  for i:=0 to WithMesh.NumMeshs-1 do begin
   CheckWithMesh(TheMesh,WithMesh.Meshs^[i]^,Finished);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
  for i:=0 to TheMesh.NumTriangles-1 do begin
   for j:=0 to WithMesh.NumTriangles-1 do begin
    CheckTriangle(@TheMesh.Triangles[i],@WithMesh.Triangles[j]);
    if Instance.NumContacts>=CollideMaxContacts then exit;
   end;
  end;
 end;
 procedure CheckTheMesh(const Sphere:TPhysicsSphere;var Mesh:TPhysicsObjectMesh);
 var i:integer;
     Finished:boolean;
 begin
  for i:=0 to Mesh.NumMeshs-1 do begin
   CheckTheMesh(Mesh.Sphere,Mesh.Meshs^[i]^);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
  Finished:=false;
  case WithObject^.CollisionBodyType of
   BodyBox,BodyCylinder:begin
    CheckWithMesh(Mesh,WithObject^.BoxMesh,Finished);
   end;
   BodyConvexHull:begin
    CheckWithMesh(Mesh,WithObject^.ConvexHullMesh,Finished);
   end;
   else begin
    for i:=0 to WithObject^.NumMeshs-1 do begin
     CheckWithMesh(Mesh,WithObject^.Meshs^[i]^,Finished);
     if Instance.NumContacts>=CollideMaxContacts then exit;
    end;
   end;
  end;
 end;
var i:integer;
begin
 if TheObject=WithObject then exit;
 TheMatrix:=Matrix4x4TermMul(WithObject^.InvTransform,TheObject^.Transform);
 case TheObject^.CollisionBodyType of
  BodyBox,BodyCylinder:begin
   CheckTheMesh(TheObject^.Sphere,TheObject^.BoxMesh);
  end;
  BodyConvexHull:begin
   CheckTheMesh(TheObject^.Sphere,TheObject^.ConvexHullMesh);
  end;
  else begin
   for i:=0 to TheObject^.NumMeshs-1 do begin
    CheckTheMesh(TheObject^.Sphere,TheObject^.Meshs^[i]^);
    if Instance.NumContacts>=CollideMaxContacts then exit;
   end;
  end;
 end;
end;

procedure PhysicsCollideObjectBoxMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if SwapObjects then begin
  PhysicsCollideObjectMeshMesh(Instance,WithObject,TheObject,ContactObject,false);
 end else begin
  PhysicsCollideObjectMeshMesh(Instance,TheObject,WithObject,ContactObject,false);
 end;
end;

procedure PhysicsCollideObjectBoxBox(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const fudgefactor:TPhysicsFloat=1.05;
type PPhysicsMatrix4x4Ex=^TPhysicsMatrix4x4Ex;
     TPhysicsMatrix4x4Ex=array[0..15] of TPhysicsFloat;
var p1,p2,p,pp,a,b,normal,normalC,ua,ub,normal2,nr,anr,Center,side1,
    side2:TPhysicsVector3;
    pa,pb,sa,sb:PPhysicsVector3;
    s,s2,l,expr1_val,Depth,sign,Alpha,Beta,c1,c2,m11,m12,m21,m22,k1,k2,k3,
    k4,det1:TPhysicsFloat;
    r1,r2:TPhysicsMatrix4x4Ex;
    ra,rb:PPhysicsMatrix4x4Ex;
    r,q:TPhysicsMatrix3x3;
    invert_normal:boolean;
    code:integer;
    i,j,lanr,a1,a2,codeN,code1,code2,n,cnum:integer;
    quad,dep:array[0..7] of TPhysicsFloat;
    rect:array[0..1] of TPhysicsFloat;
    ret:array[0..15] of TPhysicsFloat;
    points:array[0..7] of TPhysicsVector3;
    normalR:PPhysicsFloatArray;
 function DOTpq(a,b:PPhysicsFloatArray;p,q:integer):TPhysicsFloat;
 begin
  result:=(a^[0]*b^[0])+(a^[p]*b^[q])+(a^[2*p]*b^[2*q]);
 end;
 function DOT(a,b:PPhysicsFloatArray):TPhysicsFloat;
 begin
  result:=DOTpq(a,b,1,1);
 end;
 function DOT44(a,b:PPhysicsFloatArray):TPhysicsFloat;
 begin
  result:=DOTpq(a,b,4,4);
 end;
 function DOT41(a,b:PPhysicsFloatArray):TPhysicsFloat;
 begin
  result:=DOTpq(a,b,4,1);
 end;
 function DOT14(a,b:PPhysicsFloatArray):TPhysicsFloat;
 begin
  result:=DOTpq(a,b,1,4);
 end;
 procedure Mul1_331(var A:TPhysicsVector3;b,c:PPhysicsFloatArray);
 begin
  A.x:=DOT41(@b^[0],c);
  A.y:=DOT41(@b^[1],c);
  A.z:=DOT41(@b^[2],c);
 end;
 procedure Mul0_331(var A:TPhysicsVector3;b,c:PPhysicsFloatArray);
 begin
  A.x:=DOT(@b^[0],c);
  A.y:=DOT(@b^[4],c);
  A.z:=DOT(@b^[8],c);
 end;
 function intersectRectQuad:integer;
 var nq,nr,dir,sign,i:integer;
     buffer:array[0..15] of TPhysicsFloat;
     q,r,pq,pr,nextq:PPhysicsFloatArray;
 begin
  nq:=4;
  nr:=0;
  q:=@Quad[0];
  r:=@Ret[0];
  for dir:=0 to 1 do begin
   for sign:=-1 to 1 do begin
    if sign=0 then continue;
    pq:=q;
    pr:=r;
    nr:=0;
    for i:=nq downto 1 do begin
     if (sign*pq^[dir])<Rect[dir] then begin
      pr^[0]:=pq^[0];
      pr^[1]:=pq^[1];
      pr:=@pr^[2];
      inc(nr);
      if (nr and 8)<>0 then begin
       q:=r;
       if q<>@ret[0] then begin
        move(q^,ret,nr*2*sizeof(TPhysicsFloat));
       end;
       result:=nr;
       exit;
      end;
     end;
     if i>1 then begin
      nextq:=@pq^[2];
     end else begin
      nextq:=@q^[0];
     end;
     if ((sign*pq^[dir])<Rect[dir]) xor ((sign*nextq^[dir])<Rect[dir]) then begin
      pr^[1-dir]:=pq^[1-dir]+(((nextq^[1-dir]-pq^[1-dir])/(nextq^[dir]-pq^[dir]))*((sign*rect[dir])-pq^[dir]));
      pr^[dir]:=sign*rect[dir];
      pr:=@pr^[2];
      inc(nr);
      if (nr and 8)<>0 then begin
       q:=r;
       if q<>@ret[0] then begin
        move(q^,ret,nr*2*sizeof(TPhysicsFloat));
       end;
       result:=nr;
       exit;
      end;
     end;
     pq:=@pq^[2];
    end;
    q:=r;
    if q=@ret[0] then begin
     r:=@buffer[0];
    end else begin
     r:=@ret[0];
    end;
    nq:=nr;
   end;
  end;
  if q<>@ret[0] then begin
   move(q^,ret,nr*2*sizeof(TPhysicsFloat));
  end;
  result:=nr;
 end;
 function TST1(expr1,expr2:TPhysicsFloat;const norm:PPhysicsFloatArray;cc:integer):boolean;
 begin
  result:=false;
  s2:=abs(expr1)-expr2;
  if s2>0 then exit;
  if s2>s then begin
   s:=s2;
   normalR:=norm;
   invert_normal:=expr1<0;
   code:=cc;
  end;
  result:=true;
 end;
 function TST2(expr1,expr2:TPhysicsFloat;const n1,n2,n3:TPhysicsFloat;cc:integer):boolean;
 begin
  result:=false;
  s2:=abs(expr1)-expr2;
  if s2>0 then exit;
  l:=sqrt(sqr(n1)+sqr(n2)+sqr(n3));
  if l>EPSILON then begin
   l:=1.0/l;
   s2:=s2*l;
   if (s2*fudgefactor)>s then begin
    s:=s2;
    normalR:=nil;
    normalC:=Vector3ScalarMul(Vector3(n1,n2,n2),l);
    invert_normal:=expr1<0;
    code:=cc;
   end;
  end;
  result:=true;
 end;
 procedure OutputContact(const Point,Normal:TPhysicsVector3;Depth:TPhysicsFloat);
 begin
  if not PhysicsCollideAddContact(Instance,ContactObject,Point,Vector3Neg(Normal),Depth,SwapObjects,false,false) then exit;
 end;
begin
 if TheObject=WithObject then exit;
 i:=Instance.NumContacts;
 PhysicsCollideObjectMeshMesh(Instance,TheObject,WithObject,ContactObject,false);
 if i<>Instance.NumContacts then exit;
 pp:=Vector3Origin;
 normal:=Vector3Origin;
 normalC:=Vector3Origin;
 normalR:=nil;
 p1:=TheObject^.Position;
 p2:=WithObject^.Position;
 for i:=0 to 3 do begin
  for j:=0 to 3 do begin
   r1[(j*4)+i]:=TheObject^.Transform[i,j];
   r2[(j*4)+i]:=WithObject^.Transform[i,j];
  end;
 end;
 side1:=Vector3Sub(TheObject^.AABB.Max,TheObject^.AABB.Min);
 side2:=Vector3Sub(WithObject^.AABB.Max,WithObject^.AABB.Min);
 p:=Vector3Sub(p2,p1);
 Mul1_331(pp,@r1,@p);
 a:=Vector3ScalarMul(side1,0.5);
 b:=Vector3ScalarMul(side2,0.5);
 r[0,0]:=DOT44(@r1[0],@r2[0]);
 r[0,1]:=DOT44(@r1[0],@r2[1]);
 r[0,2]:=DOT44(@r1[0],@r2[2]);
 r[1,0]:=DOT44(@r1[1],@r2[0]);
 r[1,1]:=DOT44(@r1[1],@r2[1]);
 r[1,2]:=DOT44(@r1[1],@r2[2]);
 r[2,0]:=DOT44(@r1[2],@r2[0]);
 r[2,1]:=DOT44(@r1[2],@r2[1]);
 r[2,2]:=DOT44(@r1[2],@r2[2]);
 q[0,0]:=abs(r[0,0]);
 q[0,1]:=abs(r[0,1]);
 q[0,2]:=abs(r[0,2]);
 q[1,0]:=abs(r[1,0]);
 q[1,1]:=abs(r[1,1]);
 q[1,2]:=abs(r[1,2]);
 q[2,0]:=abs(r[2,0]);
 q[2,1]:=abs(r[2,1]);
 q[2,2]:=abs(r[2,2]);
 s:=-INFINITY;
 invert_normal:=false;
 code:=0;
 if not TST1(pp.x,A.x+(B.x*Q[0,0])+(B.y*Q[0,1])+(B.z*Q[0,2]),@r1[0],1) then exit;
 if not TST1(pp.y,A.y+(B.x*Q[1,0])+(B.y*Q[1,1])+(B.z*Q[1,2]),@r1[1],2) then exit;
 if not TST1(pp.z,A.z+(B.x*Q[2,0])+(B.y*Q[2,1])+(B.z*Q[2,2]),@r1[2],3) then exit;
 if not TST1(DOT41(@r2[0],@p),(A.x*Q[0,0])+(A.y*Q[1,0])+(A.z*Q[2,0])+B.x,@r2[0],4) then exit;
 if not TST1(DOT41(@r2[1],@p),(A.x*Q[0,1])+(A.y*Q[1,1])+(A.z*Q[2,1])+B.y,@r2[1],5) then exit;
 if not TST1(DOT41(@r2[2],@p),(A.x*Q[0,2])+(A.y*Q[1,2])+(A.z*Q[2,2])+B.z,@r2[2],6) then exit;
 if not TST2((pp.z*R[1,0])-(pp.y*R[2,0]),((A.y*Q[2,0])+(A.z*Q[1,0])+(B.y*Q[0,2])+(B.z*Q[0,1])),0,-R[2,0],R[1,0],7) then exit;
 if not TST2((pp.z*R[1,1])-(pp.y*R[2,1]),((A.y*Q[2,1])+(A.z*Q[1,1])+(B.x*Q[0,2])+(B.z*Q[0,0])),0,-R[2,1],R[1,1],8) then exit;
 if not TST2((pp.z*R[1,2])-(pp.y*R[2,2]),((A.y*Q[2,2])+(A.z*Q[1,2])+(B.x*Q[0,1])+(B.y*Q[0,0])),0,-R[2,2],R[1,2],9) then exit;
 if not TST2((pp.x*R[2,0])-(pp.z*R[0,0]),((A.x*Q[2,0])+(A.z*Q[0,0])+(B.y*Q[1,2])+(B.z*Q[1,1])),R[2,0],0,-R[0,0],10) then exit;
 if not TST2((pp.x*R[2,1])-(pp.z*R[0,1]),((A.x*Q[2,1])+(A.z*Q[0,1])+(B.x*Q[1,2])+(B.z*Q[1,0])),R[2,1],0,-R[0,1],11) then exit;
 if not TST2((pp.x*R[2,2])-(pp.z*R[0,2]),((A.x*Q[2,2])+(A.z*Q[0,2])+(B.x*Q[1,1])+(B.y*Q[1,0])),R[2,2],0,-R[0,2],12) then exit;
 if not TST2((pp.y*R[0,0])-(pp.x*R[1,0]),((A.x*Q[1,0])+(A.y*Q[0,0])+(B.y*Q[2,2])+(B.z*Q[2,1])),-R[1,0],R[0,0],0,13) then exit;
 if not TST2((pp.y*R[0,1])-(pp.x*R[1,1]),((A.x*Q[1,1])+(A.y*Q[0,1])+(B.x*Q[2,2])+(B.z*Q[2,0])),-R[1,1],R[0,1],0,14) then exit;
 if not TST2((pp.y*R[0,2])-(pp.x*R[1,2]),((A.x*Q[1,2])+(A.y*Q[0,2])+(B.x*Q[2,1])+(B.y*Q[2,0])),-R[1,2],R[0,2],0,15) then exit;
 if code=0 then exit;
 if assigned(normalr) then begin
  normal:=Vector3(normalR^[0],normalR^[4],normalR^[8]);
 end else begin
  Mul0_331(normal,@r1,@normalC);
 end;
 if Invert_Normal then begin
  Normal:=Vector3Neg(Normal);
 end;
 Depth:=-s;
 if Code>6 then begin
  for j:=0 to 2 do begin
   sign:=GetSignEx(Dot14(@Normal,@r1[j]));
   p1:=Vector3Add(p1,Vector3ScalarMul(Vector3(r1[j],r1[j+4],r1[j+8]),sign*A.xyz[j]));
  end;
  for j:=0 to 2 do begin
   sign:=GetSignEx(Dot14(@Normal,@r2[j]));
   p2:=Vector3Add(p2,Vector3ScalarMul(Vector3(r2[j],r2[j+4],r2[j+8]),sign*B.xyz[j]));
  end;
  i:=(Code-7) div 3;
  j:=(Code-7) mod 3;
  ua:=Vector3(r1[i],r1[i+4],r1[i+8]);
  ub:=Vector3(r2[j],r2[j+4],r2[j+8]);
  LineClosestApproach(p1,ua,p2,ub,Alpha,Beta);
  OutputContact(Vector3ScalarMul(Vector3Add(Vector3Add(p1,Vector3ScalarMul(ua,Alpha)),Vector3Add(p2,Vector3ScalarMul(ub,Beta))),0.5),Normal,Depth);
 end else begin
  if Code<=3 then begin
   Ra:=@R1;
   Rb:=@R2;
   pa:=@p1;
   pb:=@p2;
   Sa:=@A;
   Sb:=@B;
   normal2:=normal;
  end else begin
   Ra:=@R2;
   Rb:=@R1;
   pa:=@p2;
   pb:=@p1;
   Sa:=@B;
   Sb:=@A;
   normal2:=Vector3Neg(normal);
  end;
  Mul1_331(nr,@rb^[0],@normal2);
  anr.x:=abs(nr.x);
  anr.y:=abs(nr.y);
  anr.z:=abs(nr.z);
  if anr.y>anr.x then begin
   if anr.y>anr.z then begin
    a1:=0;
    lanr:=1;
    a2:=2;
   end else begin
    a1:=0;
    a2:=1;
    lanr:=2;
   end;
  end else begin
   if anr.x>anr.z then begin
    lanr:=0;
    a1:=1;
    a2:=2;
   end else begin
    a1:=0;
    a2:=1;
    lanr:=2;
   end;
  end;
  if nr.xyz[lanr]<0 then begin
   Center:=Vector3Add(Vector3Sub(pb^,pa^),Vector3ScalarMul(Vector3(Rb^[lanr],Rb^[lanr+4],Rb^[lanr+8]),Sb^.xyz[lanr]));
  end else begin
   Center:=Vector3Sub(Vector3Sub(pb^,pa^),Vector3ScalarMul(Vector3(Rb^[lanr],Rb^[lanr+4],Rb^[lanr+8]),Sb^.xyz[lanr]));
  end;
  if code<=3 then begin
   coden:=code-1;
  end else begin
   coden:=code-4;
  end;
  if coden=0 then begin
   code1:=1;
   code2:=2;
  end else if coden=1 then begin
   code1:=0;
   code2:=2;
  end else begin
   code1:=0;
   code2:=1;
  end;
  c1:=DOT14(@Center,@Ra^[code1]);
  c2:=DOT14(@Center,@Ra^[code2]);
  if Code<=3 then begin
   m11:=r[code1,a1];
   m12:=r[code1,a2];
   m21:=r[code2,a1];
   m22:=r[code2,a2];
  end else begin
   m11:=r[a1,code1];
   m12:=r[a2,code1];
   m21:=r[a1,code2];
   m22:=r[a2,code2];
  end;
{ m11:=DOT44(@Ra^[code1],@Rb^[a1]);
  m12:=DOT44(@Ra^[code1],@Rb^[a2]);
  m21:=DOT44(@Ra^[code2],@Rb^[a1]);
  m22:=DOT44(@Ra^[code2],@Rb^[a2]);}
  k1:=m11*Sb^.xyz[a1];
  k2:=m21*Sb^.xyz[a1];
  k3:=m12*Sb^.xyz[a2];
  k4:=m22*Sb^.xyz[a2];
  Quad[0]:=c1-k1-k3;
  Quad[1]:=c2-k2-k4;
  Quad[2]:=c1-k1+k3;
  Quad[3]:=c2-k2+k4;
  Quad[4]:=c1+k1+k3;
  Quad[5]:=c2+k2+k4;
  Quad[6]:=c1+k1-k3;
  Quad[7]:=c2+k2-k4;
  rect[0]:=Sa^.xyz[code1];
  rect[1]:=Sa^.xyz[code2];
  n:=IntersectRectQuad;
  if n<1 then exit;
  det1:=(m11*m22)-(m12*m21);
  if abs(det1)<EPSILON then exit;
  det1:=1/det1;
  m11:=m11*det1;
  m12:=m12*det1;
  m21:=m21*det1;
  m22:=m22*det1;
  cnum:=0;
  for j:=0 to n-1 do begin
   k1:=(m22*(ret[j*2]-c1))-(m12*(ret[(j*2)+1]-c2));
   k2:=(-(m21*(ret[j*2]-c1)))+(m11*(ret[(j*2)+1]-c2));
   points[cnum]:=Vector3Add(Center,Vector3Add(Vector3ScalarMul(Vector3(Rb^[a1],Rb^[a1+4],Rb^[a1+8]),k1),Vector3ScalarMul(Vector3(Rb^[a2],Rb^[a2+4],Rb^[a2+8]),k2)));
   dep[cnum]:=Sa^.xyz[coden]-DOT(@normal2,@points[cnum]);
   if dep[cnum]>=0 then begin
    ret[cnum*2]:=ret[j*2];
    ret[(cnum*2)+1]:=ret[(j*2)+1];
    inc(cnum);
   end;
  end;
  for j:=0 to cnum-1 do begin
   OutputContact(Vector3Add(points[j],pa^),Normal,Dep[j]);
  end;
 end;
end;

procedure PhysicsCollideObjectBoxCylinder(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var HalfLength,Radius,d,d1:TPhysicsFloat;
    t,Normal,GeometryDirection,Diff,pl,pb:TPhysicsVector3;
begin
 if TheObject=WithObject then exit;
 Radius:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 HalfLength:=abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)*0.5;
 //GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(TheObject^.Transform));
 GeometryDirection:=Vector3(TheObject^.Transform[2,0],TheObject^.Transform[2,1],TheObject^.Transform[2,2]);
 t:=Vector3ScalarMul(GeometryDirection,HalfLength);
 ClosestLineBoxPoints(Vector3Add(TheObject^.Position,t),Vector3Sub(TheObject^.Position,t),WithObject^.Position,Matrix4x4Rotation(WithObject^.InvTransform),Matrix4x4Rotation(WithObject^.Transform),Vector3Sub(WithObject^.AABB.Max,WithObject^.AABB.Min),pl,pb);
 Diff:=Vector3Sub(pb,pl);
 d:=Vector3Length(diff);
 if d>Radius then exit;
 if d<=0 then begin
  if not PhysicsCollideAddContact(Instance,ContactObject,pb,Vector3Norm(Diff),Radius,SwapObjects,false,false) then exit;
 end else begin
  d1:=1/d;
  Normal:=Vector3Norm(Vector3Sub(pb,Vector3Add(pl,Vector3ScalarMul(Diff,d1*Radius))));
  if not PhysicsCollideAddContact(Instance,ContactObject,pb,Normal,Radius-d,SwapObjects,false,false) then exit;
 end;
end;

procedure PhysicsCollideObjectBoxPlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Corners:TPhysicsBoxCorners;
    i:integer;
    Depth:TPhysicsFloat;
    Matrix:TPhysicsMatrix4x4;
begin
 if TheObject=WithObject then exit;
 Corners:=PhysicsBoxGetCorners(Vector3Sub(TheObject^.AABB.Max,TheObject^.AABB.Min));
 Matrix:=Matrix4x4TermMul(WithObject^.InvTransform,TheObject^.Transform);
 for i:=low(Corners) to high(Corners) do begin
  Depth:=-PlaneVectorDistance(WithObject^.ObjectPlane,Vector3TermMatrixMul(Corners[i],Matrix));
  if Depth>=0 then begin
   if not PhysicsCollideAddContact(Instance,ContactObject,Vector3TermMatrixMul(Corners[i],TheObject^.Transform),WithObject^.ObjectPlane.Normal,Depth,SwapObjects,false,false) then exit;
  end;
 end;
end;

procedure PhysicsCollideObjectBoxHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Corners:TPhysicsBoxCorners;
    i:integer;
    Dist:TPhysicsFloat;
    Normal:TPhysicsVector3;
begin
 if TheObject=WithObject then exit;
 Corners:=PhysicsBoxGetCorners(Vector3Sub(TheObject^.AABB.Max,TheObject^.AABB.Min));
 for i:=low(Corners) to high(Corners) do begin
  Vector3MatrixMul(Corners[i],TheObject^.Transform);
  Dist:=PhysicsObjectHeightMapGetHeightNormalDistance(WithObject^,Vector3TermMatrixMul(Corners[i],WithObject^.InvTransform),Normal);
  if Dist>0 then continue;
  Normal:=Vector3Norm(Vector3TermMatrixMul(Normal,Matrix4x4Rotation(WithObject^.Transform)));
  if not PhysicsCollideAddContact(Instance,ContactObject,Corners[i],Normal,-Dist,false,false,false) then exit;
 end;
end;

procedure PhysicsCollideObjectSphereSphere(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var r1,r2:TPhysicsFloat;
    Position,Normal:TPhysicsVector3;
begin
 if TheObject=WithObject then exit;
 r1:=TheObject^.Sphere.Radius;
 r2:=WithObject^.Sphere.Radius;
 Position:=TheObject^.Position;
 Vector3MatrixMul(Position,WithObject^.InvTransform);
 if Vector3Length(Position)<(r1+r2) then begin
  Normal:=Vector3Norm(Position);
  if not PhysicsCollideAddContact(Instance,ContactObject,Vector3ScalarMul(Normal,r1),Normal,r1+r2-Vector3Length(Position),SwapObjects) then exit;
 end;
end;

procedure PhysicsCollideObjectSphereBox(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ClosestBoxPoint,Normal:TPhysicsVector3;
    Dist,Depth:TPhysicsFloat;
begin
 if TheObject=WithObject then exit;
 Dist:=sqrt(Max(BoxGetDistanceToPoint(TheObject^.Position,Vector3Origin,Vector3Sub(WithObject^.AABB.Max,WithObject^.AABB.Min),WithObject^.InvTransform,WithObject^.Transform,ClosestBoxPoint),EPSILON));
 Depth:=TheObject^.Sphere.Radius-Dist;
 if Depth>=0 then begin
  if Dist<-EPSILON then begin
   Normal:=Vector3Norm(Vector3Sub(Vector3Sub(ClosestBoxPoint,TheObject^.Position),ClosestBoxPoint));
  end else if Dist>EPSILON then begin
   Normal:=Vector3Norm(Vector3Sub(TheObject^.Position,ClosestBoxPoint));
  end else begin
   Normal:=Vector3Norm(Vector3Sub(TheObject^.Position,WithObject^.Position));
  end;
  if not PhysicsCollideAddContact(Instance,ContactObject,ClosestBoxPoint,Normal,Depth,SwapObjects,false,false) then exit;
 end;
end;

procedure PhysicsCollideObjectSphereMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ThePosition:TPhysicsVector3;
    Radius:TPhysicsFloat;
 procedure CheckTriangle(Triangle:PPhysicsTriangle);
 var Dist,Depth,d,Dot:TPhysicsFloat;
     Normal,Point,Edge,Dir,p:TPhysicsVector3;
     i:integer;
 begin
  if not assigned(Triangle) then exit;

  Dist:=PlaneVectorDistance(Triangle^.Plane,ThePosition);
  if (Dist<0) or (Dist>=Radius) then exit;

  Normal:=Triangle^.Plane.Normal;
  Point:=Vector3Sub(ThePosition,Vector3ScalarMul(Normal,Radius));
  Depth:=Radius-Dist;

  i:=0;
  while i<3 do begin
   if PlaneVectorDistance(Triangle^.FastTriangleCheckPlanes[i],Point)<0 then break;
   inc(i);
  end;

  if i<>3 then begin
   Point:=Vector3Sub(ThePosition,Vector3ScalarMul(Normal,Dist));
   i:=0;
   while i<3 do begin
    Edge:=Vector3Sub(Triangle^.Vertices[NextTriangle[i]],Triangle^.Vertices[i]);
    Dir:=Vector3Norm(Vector3Cross(Edge,Normal));

    d:=Vector3Dot(Vector3Sub(Point,Triangle^.Vertices[i]),Dir);
    if (d>=Radius) or (d<=0) then begin
     inc(i);
     continue;
    end;

    p:=Vector3Sub(Point,Vector3ScalarMul(Dir,d));

    Dot:=Vector3Dot(p,Edge);
    if Dot>Vector3Dot(Triangle^.Vertices[NextTriangle[i]],Edge) then begin
     p:=Triangle^.Vertices[NextTriangle[i]];
    end else if Dot<Vector3Dot(Triangle^.Vertices[i],Edge) then begin
     p:=Triangle^.Vertices[i];
    end;

    d:=Vector3Length(Vector3Sub(Point,p));
    if d>Radius then begin
     inc(i);
     continue;
    end;

    Depth:=sqrt(sqr(Radius)-sqr(d))-Dist;
    if Depth<=0 then begin
     inc(i);
     continue;
    end;

    Point:=Vector3Sub(p,Vector3ScalarMul(Normal,Depth));

    break;
   end;
   if i=3 then exit;
  end;

  if not PhysicsCollideAddContact(Instance,ContactObject,Point,Normal,Depth,SwapObjects) then exit;
 end;
 procedure CheckMesh(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  if Vector3Length(Vector3Sub(Mesh.Sphere.Center,ThePosition))>(Mesh.Sphere.Radius+Radius) then exit;
  for i:=0 to Mesh.NumMeshs-1 do begin
   CheckMesh(Mesh.Meshs^[i]^);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   CheckTriangle(@Mesh.Triangles[i]);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
 end;
var i:integer;
begin
 if TheObject=WithObject then exit;

 ThePosition:=TheObject^.Position;
 Radius:=TheObject^.Sphere.Radius;
 Vector3MatrixMul(ThePosition,WithObject^.InvTransform);
 if Vector3Length(Vector3Sub(WithObject^.Sphere.Center,ThePosition))>(WithObject^.Sphere.Radius+Radius) then exit;

 case TheObject^.CollisionBodyType of
  BodyBox,BodyCylinder:begin
   CheckMesh(WithObject^.BoxMesh);
  end;
  BodyConvexHull:begin
   CheckMesh(WithObject^.ConvexHullMesh);
  end;
  else begin
   for i:=0 to WithObject^.NumMeshs-1 do begin
    CheckMesh(WithObject^.Meshs^[i]^);
    if Instance.NumContacts>=CollideMaxContacts then exit;
   end;
  end;
 end;
end;

procedure PhysicsCollideObjectSpherePlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Position,WorldPos:TPhysicsVector3;
    Dist,Depth:TPhysicsFloat;
begin
 if TheObject=WithObject then exit;
 Position:=Vector3TermMatrixMul(TheObject^.Position,WithObject^.InvTransform);
 Dist:=PlaneVectorDistance(WithObject^.ObjectPlane,Position);
 if Dist>TheObject^.Sphere.Radius then exit;
 Depth:=TheObject^.Sphere.Radius-Dist;
 WorldPos:=Vector3Sub(TheObject^.Position,Vector3ScalarMul(WithObject^.ObjectPlane.Normal,TheObject^.Sphere.Radius));
 if not PhysicsCollideAddContact(Instance,ContactObject,WorldPos,WithObject^.ObjectPlane.Normal,Depth,SwapObjects,false,false) then exit;
end;

procedure PhysicsCollideObjectSphereHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Dist,Depth:TPhysicsFloat;
    Normal,WorldPos:TPhysicsVector3;
begin
 if TheObject=WithObject then exit;
 Dist:=PhysicsObjectHeightMapGetHeightNormalDistance(WithObject^,Vector3TermMatrixMul(TheObject^.Position,WithObject^.InvTransform),Normal);
 if Dist>TheObject^.Sphere.Radius then exit;
 Normal:=Vector3Norm(Vector3TermMatrixMul(Normal,Matrix4x4Rotation(WithObject^.Transform)));
 Depth:=TheObject^.Sphere.Radius-Dist;
 WorldPos:=Vector3Sub(TheObject^.Position,Vector3ScalarMul(Normal,TheObject^.Sphere.Radius));
 if not PhysicsCollideAddContact(Instance,ContactObject,WorldPos,Normal,Depth,SwapObjects,false,false) then exit;
end;

procedure PhysicsCollideObjectCylinderMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectCapsuleMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 PhysicsCollideObjectBoxMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 // TO-DO: Add better code for flat-capped cylinder ends! (Override capsule rounding ends)
end;

procedure PhysicsCollideObjectCylinderSphere(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var CylinderRotation:TPhysicsMatrix3x3;
    CylinderPosition,CylinderAxis,CylinderPointStart,CylinderPointEnd,Diff0,
    Diff1,Project,Dir,p1,p2,Pos,Normal:TPhysicsVector3;
    CylinderRadius,CylinderSize,k,Dist,Depth:TPhysicsFloat;
begin
 if TheObject=WithObject then exit;
 CylinderRotation:=Matrix4x4GetSubMatrix3x3(Matrix4x4Rotation(TheObject^.Transform),0,0);
 CylinderPosition:=TheObject^.Position;
 CylinderRadius:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 CylinderSize:=abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)*0.5;
 CylinderAxis:=Vector3(TheObject^.Transform[2,0],TheObject^.Transform[2,1],TheObject^.Transform[2,2]);
 CylinderPointStart:=Vector3Sub(CylinderPosition,Vector3ScalarMul(CylinderAxis,CylinderSize));
 CylinderPointEnd:=Vector3Add(CylinderPosition,Vector3ScalarMul(CylinderAxis,CylinderSize));
 Diff0:=Vector3Sub(WithObject^.Position,CylinderPointStart);
 k:=Vector3Dot(Diff0,CylinderAxis);
 if (k<=(-(CylinderRadius+WithObject^.Sphere.Radius))) or (k>=((2.0*CylinderSize)+CylinderRadius+WithObject^.Sphere.Radius)) then exit;
 if k>2.0*CylinderSize then begin
  Project:=CylinderPointEnd;
 end else if k<0 then begin
  Project:=CylinderPointStart;
 end else begin
  Project:=Vector3Add(CylinderPointStart,Vector3ScalarMul(CylinderAxis,k));
 end;
 Diff1:=Vector3Sub(Project,WithObject^.Position);
 Dist:=sqrt(Vector3Dot(Diff1,Diff1));
 if Dist>(CylinderRadius+WithObject^.Sphere.Radius) then exit;
 Depth:=CylinderRadius+WithObject^.Sphere.Radius-Dist;
 if abs(Dist)>EPSILON then begin
  Dir:=Vector3ScalarMul(Diff1,1/DIST);
 end else begin
  Dir:=Vector3(TheObject^.Transform[0,0],TheObject^.Transform[0,1],TheObject^.Transform[0,2]);
 end;
 p1:=Vector3Sub(Project,Vector3ScalarMul(Dir,CylinderRadius));
 p2:=Vector3Add(WithObject^.Position,Vector3ScalarMul(Dir,CylinderRadius));
 Pos:=p1;
 Normal:=Vector3Norm(Vector3Sub(p2,p1));
 if not PhysicsCollideAddContact(Instance,ContactObject,Pos,Normal,Depth,SwapObjects,false,false) then exit;
end;

procedure PhysicsCollideObjectCylinderCylinder(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectCapsuleCapsule(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 PhysicsCollideObjectBoxBox(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 // TO-DO: Add better code for flat-capped cylinder ends! (Override capsule rounding ends)
end;

procedure PhysicsCollideObjectCylinderCapsule(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectCapsuleCapsule(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 PhysicsCollideObjectCapsuleBox(Instance,WithObject,TheObject,ContactObject,not SwapObjects);
 // TO-DO: Add better code for flat-capped cylinder ends! (Override capsule rounding ends)
end;

procedure PhysicsCollideObjectCylinderPlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectCapsulePlane(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 PhysicsCollideObjectBoxPlane(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 // TO-DO: Add better code for flat-capped cylinder ends! (Override capsule rounding ends)
end;

procedure PhysicsCollideObjectCylinderHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectCapsuleHeightMap(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 PhysicsCollideObjectBoxHeightMap(Instance,TheObject,WithObject,ContactObject,SwapObjects);
 // TO-DO: Add better code for flat-capped cylinder ends! (Override capsule rounding ends)
end;

procedure PhysicsCollideObjectCapsuleMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var CapsuleRotation:TPhysicsMatrix3x3;
    CapsulePosition,CapsuleAxis,CapsulePointStart,CapsulePointEnd,
    ThePosition:TPhysicsVector3;
    CapsuleRadius,CapsuleSize,CapsuleTolR,CapsuleTolR2:TPhysicsFloat;
 procedure CheckTriangle(Triangle:PPhysicsTriangle);
 var SegmentTriangle:TPhysicsSegmentTriangle;
     Segment:TPhysicsSegment;
     DistToStart,DistToEnd,tS,tT0,tT1,d2,Dist,Depth:TPhysicsFloat;
     Point,Normal:TPhysicsVector3;
 begin
  if not assigned(Triangle) then exit;

  DistToStart:=PlaneVectorDistance(Triangle^.TransformedPlane,CapsulePointStart);
  DistToEnd:=PlaneVectorDistance(Triangle^.TransformedPlane,CapsulePointEnd);
  if ((DistToStart>CapsuleTolR) and (DistToEnd>CapsuleTolR)) or ((DistToStart<0) and (DistToEnd<0)) then exit;

  SegmentTriangle.Origin:=Triangle^.TransformedVertices[0];
  SegmentTriangle.Edge0:=Vector3Sub(Triangle^.TransformedVertices[1],Triangle^.TransformedVertices[0]);
  SegmentTriangle.Edge1:=Vector3Sub(Triangle^.TransformedVertices[2],Triangle^.TransformedVertices[1]);
  SegmentTriangle.Edge2:=Vector3Sub(SegmentTriangle.Edge1,SegmentTriangle.Edge0);

  Segment.Origin:=CapsulePointStart;
  Segment.Delta:=Vector3Sub(CapsulePointEnd,CapsulePointStart);

  d2:=SegmentTriangleDistanceSq(tS,tT0,tT1,Segment,SegmentTriangle);
  if d2<CapsuleTolR2 then begin
   Dist:=sqrt(d2);
   Depth:=CapsuleRadius-Dist;
   Point:=Vector3Add(SegmentTriangle.Origin,Vector3Add(Vector3ScalarMul(SegmentTriangle.Edge0,tT0),Vector3ScalarMul(SegmentTriangle.Edge1,tT1)));
   if d2>EPSILON then begin
    Normal:=Vector3Norm(Vector3Sub(Vector3Add(Segment.Origin,Vector3ScalarMul(Segment.Delta,tS)),Point));
   end else begin
    Normal:=Triangle.TransformedPlane.Normal;
   end;
   if not PhysicsCollideAddContact(Instance,ContactObject,Point,Normal,Depth,SwapObjects,false,false) then exit;
  end;
 end;
 procedure CheckMesh(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  if Vector3Length(Vector3Sub(Mesh.Sphere.Center,ThePosition))>(Mesh.Sphere.Radius+TheObject^.Sphere.Radius) then exit;
  PhysicsCollideFinishObjectMesh(Instance,Mesh,WithObject^.Transform);
  for i:=0 to Mesh.NumMeshs-1 do begin
   CheckMesh(Mesh.Meshs^[i]^);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   CheckTriangle(@Mesh.Triangles[i]);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
 end;
var i:integer;
begin
 if TheObject=WithObject then exit;

 ThePosition:=TheObject^.Position;
 Vector3MatrixMul(ThePosition,WithObject^.InvTransform);
 if Vector3Length(Vector3Sub(WithObject^.Sphere.Center,ThePosition))>(WithObject^.Sphere.Radius+TheObject^.Sphere.Radius) then exit;

 CapsuleRotation:=Matrix4x4GetSubMatrix3x3(Matrix4x4Rotation(TheObject^.Transform),0,0);
 CapsulePosition:=TheObject^.Position;
 CapsuleRadius:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 CapsuleSize:=(abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)-(CapsuleRadius*2))*0.5;
 CapsuleAxis:=Vector3(TheObject^.Transform[2,0],TheObject^.Transform[2,1],TheObject^.Transform[2,2]);
 CapsulePointStart:=Vector3Sub(CapsulePosition,Vector3ScalarMul(CapsuleAxis,CapsuleSize));
 CapsulePointEnd:=Vector3Add(CapsulePosition,Vector3ScalarMul(CapsuleAxis,CapsuleSize));

 CapsuleTolR:=CapsuleRadius;
 CapsuleTolR2:=sqr(CapsuleTolR);

 for i:=0 to WithObject^.NumMeshs-1 do begin
  CheckMesh(WithObject^.Meshs^[i]^);
  if Instance.NumContacts>=CollideMaxContacts then exit;
 end;
end;

procedure PhysicsCollideObjectCapsuleSphere(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Alpha,HalfLength,r1,r2,d,d1:TPhysicsFloat;
    Position,Normal,GeometryDirection:TPhysicsVector3;
begin
 if TheObject=WithObject then exit;
 r1:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 r2:=WithObject^.Sphere.Radius;
//GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(TheObject^.Transform));
 GeometryDirection:=Vector3(TheObject^.Transform[2,0],TheObject^.Transform[2,1],TheObject^.Transform[2,2]);
 Alpha:=(GeometryDirection.x*(WithObject^.Position.x-TheObject^.Position.x))+
        (GeometryDirection.y*(WithObject^.Position.y-TheObject^.Position.y))+
        (GeometryDirection.z*(WithObject^.Position.z-TheObject^.Position.z));
 HalfLength:=(abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)-(r1*2))*0.5;
 if Alpha>HalfLength then begin
  Alpha:=HalfLength;
 end else if alpha<-HalfLength then begin
  Alpha:=-HalfLength;
 end;
 Position:=Vector3Add(TheObject^.Position,Vector3ScalarMul(GeometryDirection,Alpha));
 d:=Vector3Dist(Position,WithObject^.Position);
 if d>(r1+r2) then exit;
 if d<=0 then begin
  if not PhysicsCollideAddContact(Instance,ContactObject,Position,Vector3(1,0,0),r1+r2,SwapObjects,false,false) then exit;
 end else begin
  d1:=1/d;
  Normal:=Vector3ScalarMul(Vector3Sub(Position,WithObject^.Position),d1);
  if not PhysicsCollideAddContact(Instance,ContactObject,Vector3Add(Position,Vector3ScalarMul(Normal,(r2-r1-d)*0.5)),Normal,r1+r2-d,SwapObjects,false,false) then exit;
 end;
end;

procedure PhysicsCollideObjectCapsuleBox(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var HalfLength,Radius,d,d1:TPhysicsFloat;
    t,Normal,GeometryDirection,pl,pb:TPhysicsVector3;
begin
 if TheObject=WithObject then exit;
 Radius:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 HalfLength:=(abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)-(Radius*2))*0.5;
 //GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(TheObject^.Transform));
 GeometryDirection:=Vector3(TheObject^.Transform[2,0],TheObject^.Transform[2,1],TheObject^.Transform[2,2]);
 t:=Vector3ScalarMul(GeometryDirection,HalfLength);
 ClosestLineBoxPoints(Vector3Add(TheObject^.Position,t),Vector3Sub(TheObject^.Position,t),WithObject^.Position,Matrix4x4Rotation(WithObject^.InvTransform),Matrix4x4Rotation(WithObject^.Transform),Vector3Sub(WithObject^.AABB.Max,WithObject^.AABB.Min),pl,pb);
 d:=Vector3Dist(pl,pb);
 if d>Radius then exit;
 if d<=0 then begin
  if not PhysicsCollideAddContact(Instance,ContactObject,pl,Vector3(1,0,0),Radius,SwapObjects,false,false) then exit;
 end else begin
  d1:=1/d;
  Normal:=Vector3ScalarMul(Vector3Sub(pl,pb),d1);
  if not PhysicsCollideAddContact(Instance,ContactObject,Vector3Add(pl,Vector3ScalarMul(Normal,(-Radius-d)*0.5)),Normal,Radius-d,SwapObjects,false,false) then exit;
 end;
end;

procedure PhysicsCollideObjectCapsuleCapsule(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const tolerance=1e-5;
var GeometryDirection1,GeometryDirection2,Pos1,Pos2,Sphere1,Sphere2,
    q:TPhysicsVector3;
    Radius1,HalfLength1,Radius2,HalfLength2,a1a2,det,k,a1lo,a1hi,a2lo,a2hi,lo,
    hi,Alpha1,Alpha2,t1,t2:TPhysicsFloat;
    Seg1,Seg2:TPhysicsSegment;
 function TestSpheres(p1:TPhysicsVector3;r1:TPhysicsFloat;p2:TPhysicsVector3;r2:TPhysicsFloat):boolean;
 var d:TPhysicsFloat;
 begin
  result:=false;
  d:=Vector3Dist(p1,p2);
  if d>(r1+r2) then exit;
  result:=true;
 end;
 function CollideSpheres(p1:TPhysicsVector3;r1:TPhysicsFloat;p2:TPhysicsVector3;r2:TPhysicsFloat):boolean;
 var d,d1:TPhysicsFloat;
     Normal:TPhysicsVector3;
 begin
  result:=false; 
  d:=Vector3Dist(p1,p2);
  if d>(r1+r2) then exit;
  if d<=EPSILON then begin
   if not PhysicsCollideAddContact(Instance,WithObject,p1,Vector3(1,0,0),r1+r2,false,false,false) then exit;
  end else begin
   d1:=1/d;
   Normal:=Vector3ScalarMul(Vector3Sub(p1,p2),d1);
   if not PhysicsCollideAddContact(Instance,WithObject,Vector3Add(p1,Vector3ScalarMul(Normal,(r2-r1-d)*0.5)),Normal,r1+r2-d,SwapObjects,false,false) then exit;
  end;
  result:=true;
 end;
begin
 if TheObject=WithObject then exit;
 Radius1:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 HalfLength1:=(abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)-(Radius1*2))*0.5;
 Radius2:=Max(abs(WithObject^.AABB.Max.x-WithObject^.AABB.Min.x),abs(WithObject^.AABB.Max.y-WithObject^.AABB.Min.y))*0.5;
 HalfLength2:=(abs(WithObject^.AABB.Max.z-WithObject^.AABB.Min.z)-(Radius2*2))*0.5;
 //GeometryDirection1:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(TheObject^.Transform));
 GeometryDirection1:=Vector3(TheObject^.Transform[2,0],TheObject^.Transform[2,1],TheObject^.Transform[2,2]);
 //GeometryDirection2:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(WithObject^.Transform));
 GeometryDirection2:=Vector3(WithObject^.Transform[2,0],WithObject^.Transform[2,1],WithObject^.Transform[2,2]);
 Pos1:=TheObject^.Position;
 Pos2:=WithObject^.Position;
 a1a2:=Vector3Dot(GeometryDirection1,GeometryDirection2);
 det:=1.0-sqr(a1a2);
 if det<tolerance then begin
  if a1a2<0 then begin
   GeometryDirection2:=Vector3Neg(GeometryDirection2);
  end;
  q:=Vector3Sub(Pos1,Pos2);
  k:=Vector3Dot(GeometryDirection1,q);
  a1lo:=-HalfLength1;
  a1hi:=HalfLength1;
  a2lo:=-HalfLength2-k;
  a2hi:=HalfLength2-k;
  if a1lo>a2lo then begin
   lo:=a1lo;
  end else begin
   lo:=a2lo;
  end;
  if a1hi<a2hi then begin
   hi:=a1hi;
  end else begin
   hi:=a2hi;
  end;
  if lo<=hi then begin
   if lo<hi then begin
    Sphere1:=Vector3Add(pos1,Vector3ScalarMul(GeometryDirection1,lo));
    Sphere2:=Vector3Add(pos2,Vector3ScalarMul(GeometryDirection2,lo+k));
    if TestSpheres(Sphere1,Radius1,Sphere2,Radius2) then begin
     Sphere1:=Vector3Add(pos1,Vector3ScalarMul(GeometryDirection1,hi));
     Sphere2:=Vector3Add(pos2,Vector3ScalarMul(GeometryDirection2,hi+k));
     if TestSpheres(Sphere1,Radius1,Sphere2,Radius2) then begin
      Sphere1:=Vector3Add(pos1,Vector3ScalarMul(GeometryDirection1,lo));
      Sphere2:=Vector3Add(pos2,Vector3ScalarMul(GeometryDirection2,lo+k));
      CollideSpheres(Sphere1,Radius1,Sphere2,Radius2);
      Sphere1:=Vector3Add(pos1,Vector3ScalarMul(GeometryDirection1,hi));
      Sphere2:=Vector3Add(pos2,Vector3ScalarMul(GeometryDirection2,hi+k));
      CollideSpheres(Sphere1,Radius1,Sphere2,Radius2);
      exit;
     end;
    end;
   end;
   Alpha1:=(lo+hi)*0.5;
   Alpha2:=Alpha1+k;
   Sphere1:=Vector3Add(Pos1,Vector3ScalarMul(GeometryDirection1,Alpha1));
   Sphere2:=Vector3Add(Pos2,Vector3ScalarMul(GeometryDirection2,Alpha2));
   CollideSpheres(Sphere1,Radius1,Sphere2,Radius2);
   exit;
  end;
 end;
 Seg1.Origin:=Vector3Sub(Pos1,Vector3ScalarMul(GeometryDirection1,HalfLength1));
 Seg1.Delta:=Vector3ScalarMul(GeometryDirection1,HalfLength1*2);
 Seg2.Origin:=Vector3Sub(Pos2,Vector3ScalarMul(GeometryDirection2,HalfLength2));
 Seg2.Delta:=Vector3ScalarMul(GeometryDirection2,HalfLength2*2);
 if SegmentSegmentDistanceSq(t1,t2,Seg1,Seg2)<sqr(Radius1+Radius2) then begin
  Sphere1:=Vector3Add(Seg1.Origin,Vector3ScalarMul(Seg1.Delta,t1));
  Sphere2:=Vector3Add(Seg2.Origin,Vector3ScalarMul(Seg2.Delta,t2));
  CollideSpheres(Sphere1,Radius1,Sphere2,Radius2);
 end;
end;

procedure PhysicsCollideObjectCapsulePlane(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var GeometryDirection,p:TPhysicsVector3;
    Sign,Radius,HalfLength,k,Depth:TPhysicsFloat;
    Matrix:TPhysicsMatrix4x4;
begin
 if TheObject=WithObject then exit;
 //GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(TheObject^.Transform));
 Matrix:=Matrix4x4TermMul(WithObject^.InvTransform,TheObject^.Transform);
 GeometryDirection:=Vector3(Matrix[2,0],Matrix[2,1],Matrix[2,2]);
 if Vector3Dot(WithObject^.ObjectPlane.Normal,GeometryDirection)>0 then begin
  Sign:=-1;
 end else begin
  Sign:=1;
 end;
 Radius:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 HalfLength:=(abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)-(Radius*2))*0.5;
 p:=Vector3Add(Vector3TermMatrixMul(TheObject^.Position,WithObject^.InvTransform),Vector3ScalarMul(GeometryDirection,0.5*HalfLength*Sign));
 k:=Vector3Dot(p,WithObject^.ObjectPlane.Normal);
 Depth:=WithObject^.ObjectPlane.Distance-k+Radius;
 if Depth<0 then exit;
 if not PhysicsCollideAddContact(Instance,ContactObject,Vector3Add(p,Vector3ScalarMul(WithObject^.ObjectPlane.Normal,Radius)),WithObject^.ObjectPlane.Normal,Depth,SwapObjects,false,false) then exit;
 p:=Vector3Sub(Vector3TermMatrixMul(TheObject^.Position,WithObject^.InvTransform),Vector3ScalarMul(GeometryDirection,0.5*HalfLength*Sign));
 k:=Vector3Dot(p,WithObject^.ObjectPlane.Normal);
 Depth:=WithObject^.ObjectPlane.Distance-k+Radius;
 if Depth<0 then exit;
 if not PhysicsCollideAddContact(Instance,ContactObject,Vector3Add(p,Vector3ScalarMul(WithObject^.ObjectPlane.Normal,Radius)),WithObject^.ObjectPlane.Normal,Depth,SwapObjects,false,false) then exit;
end;

procedure PhysicsCollideObjectCapsuleHeightMap(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var GeometryDirection,StartPos,EndPos,MiddlePos,Normal,WorldPos:TPhysicsVector3;
    Radius,HalfLength,Dist,Depth:TPhysicsFloat;
begin
 if TheObject=WithObject then exit;
 //GeometryDirection:=Vector3TermMatrixMul(Vector3(0,0,1),Matrix4x4Rotation(TheObject^.Transform));
 GeometryDirection:=Vector3(TheObject^.Transform[2,0],TheObject^.Transform[2,1],TheObject^.Transform[2,2]);
 Radius:=Max(abs(TheObject^.AABB.Max.x-TheObject^.AABB.Min.x),abs(TheObject^.AABB.Max.y-TheObject^.AABB.Min.y))*0.5;
 HalfLength:=(abs(TheObject^.AABB.Max.z-TheObject^.AABB.Min.z)-(Radius*2))*0.5;
 StartPos:=Vector3Sub(TheObject^.Position,Vector3ScalarMul(GeometryDirection,0.5*HalfLength));
 EndPos:=Vector3Add(TheObject^.Position,Vector3ScalarMul(GeometryDirection,0.5*HalfLength));
 MiddlePos:=Vector3ScalarMul(Vector3Add(StartPos,EndPos),0.5);
 Dist:=PhysicsObjectHeightMapGetHeightNormalDistance(WithObject^,Vector3TermMatrixMul(StartPos,WithObject^.InvTransform),Normal);
 if Dist<Radius then begin
  Normal:=Vector3Norm(Vector3TermMatrixMul(Normal,Matrix4x4Rotation(WithObject^.Transform)));
  Depth:=Radius-Dist;
  WorldPos:=Vector3Sub(StartPos,Vector3ScalarMul(Normal,Radius));
  if not PhysicsCollideAddContact(Instance,ContactObject,WorldPos,Normal,Depth,SwapObjects,false,false) then exit;
 end;
 Dist:=PhysicsObjectHeightMapGetHeightNormalDistance(WithObject^,Vector3TermMatrixMul(MiddlePos,WithObject^.InvTransform),Normal);
 if Dist<Radius then begin
  Normal:=Vector3Norm(Vector3TermMatrixMul(Normal,Matrix4x4Rotation(WithObject^.Transform)));
  Depth:=Radius-Dist;
  WorldPos:=Vector3Sub(MiddlePos,Vector3ScalarMul(Normal,Radius));
  if not PhysicsCollideAddContact(Instance,ContactObject,WorldPos,Normal,Depth,SwapObjects,false,false) then exit;
 end;
 Dist:=PhysicsObjectHeightMapGetHeightNormalDistance(WithObject^,Vector3TermMatrixMul(EndPos,WithObject^.InvTransform),Normal);
 if Dist<Radius then begin
  Normal:=Vector3Norm(Vector3TermMatrixMul(Normal,Matrix4x4Rotation(WithObject^.Transform)));
  Depth:=Radius-Dist;
  WorldPos:=Vector3Sub(EndPos,Vector3ScalarMul(Normal,Radius));
  if not PhysicsCollideAddContact(Instance,ContactObject,WorldPos,Normal,Depth,SwapObjects,false,false) then exit;
 end;
end;

procedure PhysicsCollideObjectPlaneMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ThePosition:TPhysicsVector3;
    TheMatrix:TPhysicsMatrix4x4;
    TheMatrix2:TPhysicsMatrix4x4;
    TheMatrix3:TPhysicsMatrix4x4;
 procedure CheckTriangle(Triangle:PPhysicsTriangle);
 const c1d3=1/3;
 var Depth:TPhysicsFloat;
     Point,Normal:TPhysicsVector3;
     i:integer;
 begin
  if not assigned(Triangle) then exit;

  for i:=0 to 2 do begin
   Point:=Triangle^.TransformedVertices[i];

   Depth:=TheObject^.ObjectPlane.Distance-Vector3Dot(TheObject^.ObjectPlane.Normal,Point);
   if Depth>0 then begin
    Vector3MatrixMul(Point,TheMatrix2);
    Normal:=Vector3Norm(Vector3TermMatrixMul(TheObject^.ObjectPlane.Normal,TheMatrix3));
    if not PhysicsCollideAddContact(Instance,ContactObject,Point,Normal,Depth,SwapObjects,false,false) then exit;
   end;
  end;
 end;
 procedure CheckMesh(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  if Vector3Length(Vector3Sub(Mesh.Sphere.Center,ThePosition))>(Mesh.Sphere.Radius+TheObject^.Sphere.Radius) then exit;
  PhysicsCollideFinishObjectMesh(Instance,Mesh,TheMatrix);
  for i:=0 to Mesh.NumMeshs-1 do begin
   CheckMesh(Mesh.Meshs^[i]^);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   CheckTriangle(@Mesh.Triangles[i]);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
 end;
var i:integer;
begin
 if TheObject=WithObject then exit;

 ThePosition:=TheObject^.Position;
 Vector3MatrixMul(ThePosition,WithObject^.InvTransform);
 if Vector3Length(Vector3Sub(WithObject^.Sphere.Center,ThePosition))>(WithObject^.Sphere.Radius+TheObject^.Sphere.Radius) then exit;

 TheMatrix:=Matrix4x4TermMul(WithObject^.Transform,TheObject^.InvTransform);
 TheMatrix2:=Matrix4x4TermMul(WithObject^.InvTransform,TheObject^.Transform);
 TheMatrix3:=Matrix4x4Rotation(TheMatrix2);

 for i:=0 to WithObject^.NumMeshs-1 do begin
  CheckMesh(WithObject^.Meshs^[i]^);
  if Instance.NumContacts>=CollideMaxContacts then exit;
 end;
end;

procedure PhysicsCollideObjectHeightMapMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var ThePosition:TPhysicsVector3;
    TheMatrix:TPhysicsMatrix4x4;
 procedure CheckTriangle(Triangle:PPhysicsTriangle);
 const c1d3=1/3;
 var Dist:TPhysicsFloat;
     Point,Normal:TPhysicsVector3;
     i:integer;
 begin
  if not assigned(Triangle) then exit;

  for i:=0 to 2 do begin
   Point:=Triangle^.TransformedVertices[i];

   Dist:=PhysicsObjectHeightMapGetHeightNormalDistance(TheObject^,Point,Normal);
   if Dist>0 then continue;
                          
   Vector3MatrixMul(Point,TheObject^.Transform);
   Normal:=Vector3Norm(Vector3TermMatrixMul(Normal,TheObject^.Transform));
   if not PhysicsCollideAddContact(Instance,ContactObject,Point,Normal,Dist,SwapObjects,false,false) then exit;
  end;
 end;
 procedure CheckMesh(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  if Vector3Length(Vector3Sub(Mesh.Sphere.Center,ThePosition))>(Mesh.Sphere.Radius+TheObject^.Sphere.Radius) then exit;
  PhysicsCollideFinishObjectMesh(Instance,Mesh,TheMatrix);
  for i:=0 to Mesh.NumMeshs-1 do begin
   CheckMesh(Mesh.Meshs^[i]^);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   CheckTriangle(@Mesh.Triangles[i]);
   if Instance.NumContacts>=CollideMaxContacts then exit;
  end;
 end;
var i:integer;
begin
 if TheObject=WithObject then exit;

 ThePosition:=TheObject^.Position;
 Vector3MatrixMul(ThePosition,WithObject^.InvTransform);
 if Vector3Length(Vector3Sub(WithObject^.Sphere.Center,ThePosition))>(WithObject^.Sphere.Radius+TheObject^.Sphere.Radius) then exit;

 TheMatrix:=Matrix4x4TermMul(WithObject^.Transform,TheObject^.InvTransform);

 for i:=0 to WithObject^.NumMeshs-1 do begin
  CheckMesh(WithObject^.Meshs^[i]^);
  if Instance.NumContacts>=CollideMaxContacts then exit;
 end;
end;

procedure PhysicsCollideObjectConvexHullMesh(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectMeshMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

procedure PhysicsCollideObjectBoxConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectBoxMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

procedure PhysicsCollideObjectSphereConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectSphereMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

procedure PhysicsCollideObjectCylinderConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectCylinderMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

procedure PhysicsCollideObjectCapsuleConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectCapsuleMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

procedure PhysicsCollideObjectPlaneConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectPlaneMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

procedure PhysicsCollideObjectHeightMapConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectHeightMapMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

procedure PhysicsCollideObjectConvexHullConvexHull(var Instance:TPhysicsCollide;TheObject,WithObject,ContactObject:PPhysicsObject;SwapObjects:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if TheObject=WithObject then exit;
 PhysicsCollideObjectMeshMesh(Instance,TheObject,WithObject,ContactObject,SwapObjects);
end;

const CollideProcs:TPhysicsCollideProcs=(
                                         (// BodyMesh
                                          (Proc:PhysicsCollideObjectMeshMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectBoxMesh;SwapObjects:true),// BodyBox
                                          (Proc:PhysicsCollideObjectSphereMesh;SwapObjects:true),// BodySphere
                                          (Proc:PhysicsCollideObjectCylinderMesh;SwapObjects:true),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCapsuleMesh;SwapObjects:true),// BodyCapsule
                                          (Proc:PhysicsCollideObjectPlaneMesh;SwapObjects:true),// BodyPlane
                                          (Proc:PhysicsCollideObjectHeightMapMesh;SwapObjects:true),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectMeshMesh;SwapObjects:false) // BodyConvexHull
                                         ),
                                         (// BodyBox
                                          (Proc:PhysicsCollideObjectBoxMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectBoxBox;SwapObjects:false),// BodyBox
                                          (Proc:PhysicsCollideObjectSphereBox;SwapObjects:true),// BodySphere
                                          (Proc:PhysicsCollideObjectBoxCylinder;SwapObjects:false),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCapsuleBox;SwapObjects:true),// BodyCapsule
                                          (Proc:PhysicsCollideObjectBoxPlane;SwapObjects:false),// BodyPlane
                                          (Proc:PhysicsCollideObjectBoxHeightMap;SwapObjects:false),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectBoxMesh;SwapObjects:false) // BodyConvexHull
                                         ),
                                         (// BodySphere
                                          (Proc:PhysicsCollideObjectSphereMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectSphereBox;SwapObjects:false),// BodyBox
                                          (Proc:PhysicsCollideObjectSphereSphere;SwapObjects:false),// BodySphere
                                          (Proc:PhysicsCollideObjectCylinderSphere;SwapObjects:true),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCapsuleSphere;SwapObjects:true),// BodyCapsule
                                          (Proc:PhysicsCollideObjectSpherePlane;SwapObjects:false),// BodyPlane
                                          (Proc:PhysicsCollideObjectSphereHeightMap;SwapObjects:false),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectSphereMesh;SwapObjects:false) // BodyConvexHull
                                         ),
                                         (// BodyCylinder
                                          (Proc:PhysicsCollideObjectCylinderMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectBoxCylinder;SwapObjects:true),// BodyBox
                                          (Proc:PhysicsCollideObjectCylinderSphere;SwapObjects:false),// BodySphere
                                          (Proc:PhysicsCollideObjectCylinderCylinder;SwapObjects:false),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCylinderCapsule;SwapObjects:false),// BodyCapsule
                                          (Proc:PhysicsCollideObjectCylinderPlane;SwapObjects:false),// BodyPlane
                                          (Proc:PhysicsCollideObjectCylinderHeightMap;SwapObjects:false),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectCylinderMesh;SwapObjects:false) // BodyConvexHull
                                         ),
                                         (// BodyCapsule
                                          (Proc:PhysicsCollideObjectCapsuleMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectCapsuleBox;SwapObjects:false),// BodyBox
                                          (Proc:PhysicsCollideObjectCapsuleSphere;SwapObjects:false),// BodySphere
                                          (Proc:PhysicsCollideObjectCylinderCapsule;SwapObjects:true),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCapsuleCapsule;SwapObjects:false),// BodyCapsule
                                          (Proc:PhysicsCollideObjectCapsulePlane;SwapObjects:false),// BodyPlane
                                          (Proc:PhysicsCollideObjectCapsuleHeightMap;SwapObjects:false),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectCapsuleMesh;SwapObjects:false) // BodyConvexHull
                                         ),
                                         (// BodyPlane
                                          (Proc:PhysicsCollideObjectPlaneMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectBoxPlane;SwapObjects:true),// BodyBox
                                          (Proc:PhysicsCollideObjectSpherePlane;SwapObjects:true),// BodySphere
                                          (Proc:PhysicsCollideObjectCylinderPlane;SwapObjects:true),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCapsulePlane;SwapObjects:true),// BodyCapsule
                                          (Proc:PhysicsCollideObjectPlaneMesh;SwapObjects:false),// BodyPlane
                                          (Proc:PhysicsCollideObjectPlaneMesh;SwapObjects:false),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectPlaneMesh;SwapObjects:false) // BodyConvexHull
                                         ),
                                         (// BodyHeightmap
                                          (Proc:PhysicsCollideObjectHeightMapMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectBoxHeightMap;SwapObjects:true),// BodyBox
                                          (Proc:PhysicsCollideObjectSphereHeightMap;SwapObjects:true),// BodySphere
                                          (Proc:PhysicsCollideObjectCylinderHeightMap;SwapObjects:true),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCapsuleHeightMap;SwapObjects:true),// BodyCapsule
                                          (Proc:PhysicsCollideObjectHeightMapMesh;SwapObjects:false),// BodyPlane
                                          (Proc:PhysicsCollideObjectHeightMapMesh;SwapObjects:false),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectHeightMapConvexHull;SwapObjects:false) // BodyConvexHull
                                         ),
                                         (// BodyConvexHull
                                          (Proc:PhysicsCollideObjectConvexHullMesh;SwapObjects:false),// BodyMesh
                                          (Proc:PhysicsCollideObjectBoxConvexHull;SwapObjects:true),// BodyBox
                                          (Proc:PhysicsCollideObjectSphereConvexHull;SwapObjects:true),// BodySphere
                                          (Proc:PhysicsCollideObjectCylinderConvexHull;SwapObjects:true),// BodyCylinder
                                          (Proc:PhysicsCollideObjectCapsuleConvexHull;SwapObjects:false),// BodyCapsule
                                          (Proc:PhysicsCollideObjectPlaneConvexHull;SwapObjects:false),// BodyPlane
                                          (Proc:PhysicsCollideObjectHeightMapConvexHull;SwapObjects:false),// BodyHeightmap
                                          (Proc:PhysicsCollideObjectConvexHullConvexHull;SwapObjects:false) // BodyConvexHull
                                         )
                                        );

function PhysicsContactCompare(const a,b:pointer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var DA:PPhysicsContact absolute a;
    DB:PPhysicsContact absolute b;
begin
 if DA^.ContactObject^.Position.y>DB^.ContactObject^.Position.y then begin
  result:=-1;
 end else if DA^.ContactObject^.Position.y<DB^.ContactObject^.Position.y then begin
  result:=1;
 end else if DA^.Point.y>DB^.Point.y then begin
  result:=-1;
 end else if DA^.Point.y<DB^.Point.y then begin
  result:=1;
 end else begin
  result:=0;
 end;
end;

procedure PhysicsCollideSort(var Instance:TPhysicsCollide); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  PhysicsSort(Contacts,NumContacts,PhysicsContactCompare);
 end;
end;

function PhysicsCollideGetContactCount(var Instance:TPhysicsCollide):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.NumContacts;
end;

function PhysicsCollideGetContact(var Instance:TPhysicsCollide;Number:integer;var Point,Normal:TPhysicsVector3;var Depth:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if (Number>=0) and (Number<Instance.NumContacts) then begin
  Point:=Instance.Contacts^[Number]^.Point;
  Normal:=Instance.Contacts^[Number]^.Normal;
  Depth:=Instance.Contacts^[Number]^.Depth;
  result:=true;
 end else begin
  result:=false;
 end;
end;

function PhysicsCollide(var Instance:TPhysicsCollide;TheObject:PPhysicsObject):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
const FlagALLAXIS=(1 shl 0) or (1 shl 1) or (1 shl 2);
var MustResort:boolean;
    CurrentObject:PPhysicsObject;
 procedure ProcessObject(ObjectToCheck:PPhysicsObject);
 var i:integer;
     RigidBody:PPhysicsRigidBody;
     CollideProc:PPhysicsCollideProc;
 begin
  if (((TheObject=ObjectToCheck) or not assigned(ObjectToCheck)) or
     (not (TheObject^.CollisionReceiver and ObjectToCheck^.CollisionSender))) or
     (abs(Vector3Length(Vector3Sub(Vector3Add(TheObject^.Position,TheObject^.Sphere.Center),Vector3Add(ObjectToCheck^.Position,ObjectToCheck^.Sphere.Center))))>(TheObject^.Sphere.Radius+ObjectToCheck^.Sphere.Radius)) then begin
   exit;
  end;
  if assigned(TheObject^.RigidBody) and assigned(ObjectToCheck^.RigidBody) then begin
   RigidBody:=TheObject^.RigidBody;
   i:=0;
   while i<RigidBody^.NumJoints do begin
    if RigidBody^.JointedRigidBodies^[i]=ObjectToCheck^.RigidBody then begin
     break;
    end;
    inc(i);
   end;
   if i<RigidBody^.NumJoints then begin
    exit;
   end;
  end;
  if not (assigned(Instance.UserCollideProc) and Instance.UserCollideProc(@Instance,TheObject,ObjectToCheck)) then begin
   if (TheObject^.CollisionBodyType in [BodyStart..BodyEnd]) and (ObjectToCheck^.CollisionBodyType in [BodyStart..BodyEnd]) then begin
    CollideProc:=@CollideProcs[TheObject^.CollisionBodyType,ObjectToCheck^.CollisionBodyType];
    if CollideProc^.SwapObjects then begin
     CollideProc^.Proc(Instance,ObjectToCheck,TheObject,ObjectToCheck,true);
    end else begin
     CollideProc^.Proc(Instance,TheObject,ObjectToCheck,ObjectToCheck,false);
    end;
   end;
  end;
 end;
var i:integer;
begin
 with Instance do begin
  NumObjects:=0;
  NumContacts:=0;

  if not TheObject^.CollisionReceiver then begin
   result:=0;
   exit;
  end;

  case PhysicsInstance^.SweepAndPruneWorkMode of
   sapwmAXISALL:begin
    for i:=0 to TheObject^.SweepAndPrunePairsCount-1 do begin
     if TheObject^.SweepAndPrunePairs^[i].Flags=FlagALLAXIS then begin
      ProcessObject(TheObject^.SweepAndPrunePairs^[i].WithObject);
     end;
    end;
   end;
   sapwmOFF:begin
    CurrentObject:=PhysicsInstance^.ObjectFirst;
    while assigned(CurrentObject) do begin
     if TheObject<>CurrentObject then begin
      ProcessObject(CurrentObject);
     end;
     CurrentObject:=CurrentObject^.Next;
    end;
   end;
   else begin
    for i:=0 to TheObject^.SweepAndPrunePairsCount-1 do begin
     ProcessObject(TheObject^.SweepAndPrunePairs^[i].WithObject);
    end;
   end;
  end;

  result:=NumContacts;
 end;
 PhysicsCollideSort(Instance);
 if assigned(TheObject^.OnCollision) then begin
  MustResort:=false;
  for i:=0 to Instance.NumContacts-1 do begin
   if TheObject^.OnCollision(TheObject,Instance.Contacts^[i]^) then begin
    MustResort:=true;
   end;
  end;
  if MustResort then begin
   PhysicsCollideSort(Instance);
  end;
 end;
end;

function PhysicsCollidePoint(var Instance:TPhysicsCollide;Point:TPhysicsVector3;Radius:TPhysicsFloat):integer; overload;
 procedure ProcessObject(ObjectToCheck:PPhysicsObject);
 begin
  if not assigned(ObjectToCheck) then exit;
  if not ObjectToCheck^.CollisionSender then exit;
  if Vector3Length(Vector3Sub(Point,Vector3Add(ObjectToCheck^.Position,ObjectToCheck^.Sphere.Center)))>(Radius+ObjectToCheck^.Sphere.Radius) then exit;
  if not (assigned(Instance.UserPointCollideProc) and Instance.UserPointCollideProc(@Instance,ObjectToCheck,Point,Radius)) then begin
   case ObjectToCheck^.CollisionBodyType of
    BodyStart..BodyEnd:begin
     PhysicsCollidePointProcs[ObjectToCheck^.CollisionBodyType](Instance,ObjectToCheck,Point,Radius);
    end;
    else begin
    end;
   end;
  end;
 end;
var CurObj:PPhysicsObject;
begin
 with Instance do begin
  NumObjects:=0;
  NumContacts:=0;

  CurObj:=Physicsinstance.ObjectFirst;
  while assigned(CurObj) do begin
   ProcessObject(CurObj);
   CurObj:=CurObj^.Next;
  end;

  result:=NumContacts;
 end;
end;

////////////////////////////////////// Joint ///////////////////////////////////

procedure PhysicsJointInit(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j:integer;
begin
 fillchar(Instance,sizeof(TPhysicsJoint),#0);
 with Instance do begin
  JointType:=JointNone;

  RigidBodies[0]:=RigidBody0;
  RigidBodies[1]:=RigidBody1;

  HaveDestroyMinVelocity:=false;
  DestroyMinVelocity:=0;

  Active:=true;

  for i:=0 to 1 do begin
   inc(RigidBodies[i]^.NumJoints);
   j:=(RigidBodies[i]^.NumJoints+MemoryInc) and not MemoryIncMask;
   if j<>RigidBodies[i]^.AllJoints then begin
    RigidBodies[i]^.AllJoints:=j;
    PhysicsReallocateMemory(RigidBodies[i]^.Joints,RigidBodies[i]^.AllJoints*sizeof(PPhysicsJoint));
    PhysicsReallocateMemory(RigidBodies[i]^.JointedRigidBodies,RigidBodies[i]^.AllJoints*sizeof(PPhysicsRigidBody));
   end;
   RigidBodies[i]^.Joints^[RigidBodies[i]^.NumJoints-1]:=@Instance;
   RigidBodies[i]^.JointedRigidBodies^[RigidBodies[i]^.NumJoints-1]:=RigidBodies[1-i];
  end;
 end;
 inc(PhysicsInstance^.NumJoints);
 j:=(PhysicsInstance^.NumJoints+MemoryInc) and not MemoryIncMask;
 if j<>PhysicsInstance^.AllJoints then begin
  PhysicsInstance^.AllJoints:=j;
  PhysicsReallocateMemory(PhysicsInstance^.Joints,PhysicsInstance^.AllJoints*sizeof(PPhysicsJoint));
 end;
 PhysicsInstance^.Joints^[PhysicsInstance^.NumJoints-1]:=@Instance;
end;

procedure PhysicsJointDone(var Instance:TPhysicsJoint); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j,k:integer;
begin
 with Instance do begin
  for i:=0 to 1 do begin
   k:=-1;
   for j:=0 to RigidBodies[i]^.NumJoints-1 do begin
    if RigidBodies[i]^.Joints[j]=@Instance then begin
     k:=j;
     break;
    end;
   end;
   if k>=0 then begin
    dec(RigidBodies[i]^.NumJoints);
    move(RigidBodies[i]^.Joints[k+1],RigidBodies[i]^.Joints[k],RigidBodies[i]^.NumJoints*sizeof(PPhysicsJoint));
    move(RigidBodies[i]^.JointedRigidBodies[k+1],RigidBodies[i]^.JointedRigidBodies[k],RigidBodies[i]^.NumJoints*sizeof(PPhysicsRigidBody));
    j:=(RigidBodies[i]^.NumJoints+MemoryInc) and not MemoryIncMask;
    if j<>RigidBodies[i]^.AllJoints then begin
     RigidBodies[i]^.AllJoints:=j;
     PhysicsReallocateMemory(RigidBodies[i]^.Joints,RigidBodies[i]^.AllJoints*sizeof(PPhysicsJoint));
     PhysicsReallocateMemory(RigidBodies[i]^.JointedRigidBodies,RigidBodies[i]^.AllJoints*sizeof(PPhysicsRigidBody));
    end;
   end;
  end;

  k:=-1;
  for j:=0 to PhysicsInstance^.NumJoints-1 do begin
   if PhysicsInstance^.Joints[j]=@Instance then begin
    k:=j;
    break;
   end;
  end;
  if k>=0 then begin
   dec(PhysicsInstance^.NumJoints);
   move(PhysicsInstance^.Joints[k+1],PhysicsInstance^.Joints[k],PhysicsInstance^.NumJoints*sizeof(PPhysicsJoint));
   j:=(PhysicsInstance^.NumJoints+MemoryInc) and not MemoryIncMask;
   if j<>PhysicsInstance^.AllJoints then begin
    PhysicsInstance^.AllJoints:=j;
    PhysicsReallocateMemory(PhysicsInstance^.Joints,PhysicsInstance^.AllJoints*sizeof(PPhysicsJoint));
   end;
  end;

 end;
 fillchar(Instance,sizeof(TPhysicsJoint),#0);
end;

procedure PhysicsJointSetActive(var Instance:TPhysicsJoint;Active:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Instance.Active:=Active;
end;

function PhysicsJointIsActive(var Instance:TPhysicsJoint):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.Active;
end;

procedure PhysicsJointSetDestroyMinVelocity(var Instance:TPhysicsJoint;Active:boolean;DestroyMinVelocity:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Instance.HaveDestroyMinVelocity:=Active;
 Instance.DestroyMinVelocity:=DestroyMinVelocity;
end;

function PhysicsJointRestrictionResponse(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat;const Point0,Point1:TPhysicsVector3;MinDist:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var p,r,v:array[0..1] of TPhysicsVector3;
    Normal,Vel:TPhysicsVector3;
    Dist,NormalVel,ImpulseNumerator,ImpulseDenominator:TPhysicsFloat;
begin
 with Instance do begin
  if MinDist=(JOINT_DIST*2) then begin
   result:=true;
   exit;
  end;

  p[0]:=Vector3TermMatrixMul(Point0,RigidBodies[0]^.Transform);
  p[1]:=Vector3TermMatrixMul(Point1,RigidBodies[1]^.Transform);

  r[0]:=Vector3Sub(p[0],RigidBodies[0]^.Position);
  r[1]:=Vector3Sub(p[1],RigidBodies[1]^.Position);

  v[0]:=Vector3Add(Vector3Cross(RigidBodies[0]^.AngularVelocity,r[0]),RigidBodies[0]^.Velocity);
  v[1]:=Vector3Add(Vector3Cross(RigidBodies[1]^.AngularVelocity,r[1]),RigidBodies[1]^.Velocity);

  Normal:=Vector3Add(Vector3Sub(p[0],p[1]),Vector3Sub(Vector3ScalarMul(v[0],TimeToWork),Vector3ScalarMul(v[1],TimeToWork)));
  Dist:=Vector3Length(Normal);
  if Dist>=MinDist then begin
   result:=true;
   exit;
  end;

  Vel:=Vector3Sub(v[0],v[1]);
  NormalVel:=Vector3Dot(Normal,Vel);

  if HaveDestroyMinVelocity then begin
   if NormalVel>DestroyMinVelocity then begin
    result:=true;
    Active:=false;
    exit;
   end;
  end;

  ImpulseNumerator:=-NormalVel+((MinDist-Dist)*(PhysicsInstance^.PenetrationSpeed/TimeToWork));
  ImpulseDenominator:=RigidBodies[0]^.InvMass+
                      RigidBodies[1]^.InvMass+
                      Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[0],Normal),RigidBodies[0]^.InvWorldInertiaTensor),r[0]))+
                      Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[1],Normal),RigidBodies[1]^.InvWorldInertiaTensor),r[1]));

  if ImpulseDenominator<EPSILON then begin
   result:=false;
   exit;
  end;

  if not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
  end;
  if not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
  end;

  if (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) and not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
  end;
  if (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) and not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
  end;

  result:=false;
 end;
end;

////////////////////////////////// Joint Base //////////////////////////////////

procedure PhysicsJointInitBase(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsJointInit(Instance,RigidBody0,RigidBody1);
 with Instance do begin
  JointType:=JointBase;

  RestrictionPoints[0]:=Vector3TermMatrixMul(Vector3Add(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis0),JOINT_DIST)),RigidBody0^.InvTransform);
  RestrictionPoints[1]:=Vector3TermMatrixMul(Vector3Sub(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis1),JOINT_DIST)),RigidBody1^.InvTransform);

  RestrictionMinDist:=sqrt(2*sqr(JOINT_DIST)*(1-cos(RestrictionAngle*DEG2RAD)));
 end;
end;

function PhysicsJointResponseBase(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  PhysicsJointRestrictionResponse(Instance,TimeToWork,RestrictionPoints[0],RestrictionPoints[1],RestrictionMinDist);
  result:=false;
 end;
end;

////////////////////////////////// Joint Ball //////////////////////////////////

procedure PhysicsJointInitBall(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsJointInit(Instance,RigidBody0,RigidBody1);
 with Instance do begin
  JointType:=JointBall;

  BallPoints[0]:=Vector3TermMatrixMul(Point,RigidBody0^.InvTransform);
  BallPoints[1]:=Vector3TermMatrixMul(Point,RigidBody1^.InvTransform);

  RestrictionPoints[0]:=Vector3TermMatrixMul(Vector3Add(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis0),JOINT_DIST)),RigidBody0^.InvTransform);
  RestrictionPoints[1]:=Vector3TermMatrixMul(Vector3Sub(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis1),JOINT_DIST)),RigidBody1^.InvTransform);

  RestrictionMinDist:=sqrt(2*sqr(JOINT_DIST)*(1-cos(RestrictionAngle*DEG2RAD)));
 end;
end;

function PhysicsJointResponseBall(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var p,r,v:array[0..1] of TPhysicsVector3;
    Normal,Vel:TPhysicsVector3;
    NormalVel,ImpulseNumerator,ImpulseDenominator:TPhysicsFloat;
begin
 with Instance do begin
  p[0]:=Vector3TermMatrixMul(BallPoints[0],RigidBodies[0]^.Transform);
  p[1]:=Vector3TermMatrixMul(BallPoints[1],RigidBodies[1]^.Transform);

  r[0]:=Vector3Sub(p[0],RigidBodies[0]^.Position);
  r[1]:=Vector3Sub(p[1],RigidBodies[1]^.Position);

  v[0]:=Vector3Add(Vector3Cross(RigidBodies[0]^.AngularVelocity,r[0]),RigidBodies[0]^.Velocity);
  v[1]:=Vector3Add(Vector3Cross(RigidBodies[1]^.AngularVelocity,r[1]),RigidBodies[1]^.Velocity);

  Vel:=Vector3Add(Vector3ScalarMul(Vector3Sub(p[0],p[1]),PhysicsInstance^.PenetrationSpeed/TimeToWork),Vector3Sub(v[0],v[1]));

  NormalVel:=Vector3Length(Vel);
  if NormalVel<EPSILON then begin
   result:=true;
   exit;
  end else if NormalVel>PhysicsInstance^.VelocityMax then begin
   NormalVel:=PhysicsInstance^.VelocityMax;
  end;

  if HaveDestroyMinVelocity then begin
   if NormalVel>DestroyMinVelocity then begin
    result:=true;
    Active:=false;
    exit;
   end;
  end;

  Normal:=Vector3Norm(Vel);

  ImpulseNumerator:=-NormalVel;
  ImpulseDenominator:=RigidBodies[0]^.InvMass+
                      RigidBodies[1]^.InvMass+
                      Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[0],Normal),RigidBodies[0]^.InvWorldInertiaTensor),r[0]))+
                      Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[1],Normal),RigidBodies[1]^.InvWorldInertiaTensor),r[1]));

  if ImpulseDenominator<EPSILON then begin
   result:=false;
   exit;
  end;

  if not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
  end;
  if not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
  end;

  if (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) and not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
  end;
  if (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) and not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
   PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
  end;

  PhysicsJointRestrictionResponse(Instance,TimeToWork,RestrictionPoints[0],RestrictionPoints[1],RestrictionMinDist);

  result:=false;
 end;
end;

////////////////////////////////// Joint Hinge /////////////////////////////////

procedure PhysicsJointInitHinge(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,Axis,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsJointInit(Instance,RigidBody0,RigidBody1);
 with Instance do begin
  JointType:=JointHinge;

  HingePoint:=Point;

  HingeiTransforms[0]:=RigidBody0^.InvTransform;
  HingeiTransforms[1]:=RigidBody1^.InvTransform;

  HingePoints[0,0]:=Vector3TermMatrixMul(Point,RigidBody0^.InvTransform);
  HingePoints[0,1]:=Vector3TermMatrixMul(Point,RigidBody1^.InvTransform);

  PhysicsJointHingeSetAxis(Instance,0,Axis);
  PhysicsJointHingeSetAxis(Instance,1,Axis);

  RestrictionPoints[0]:=Vector3TermMatrixMul(Vector3Add(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis0),JOINT_DIST)),RigidBody0^.InvTransform);
  RestrictionPoints[1]:=Vector3TermMatrixMul(Vector3Sub(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis1),JOINT_DIST)),RigidBody1^.InvTransform);

  RestrictionMinDist:=sqrt(2*sqr(JOINT_DIST)*(1-cos(RestrictionAngle*DEG2RAD)));
 end;
end;

procedure PhysicsJointHingeSetAxis(var Instance:TPhysicsJoint;Number:integer;const Axis:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  HingeAxis[Number]:=Vector3Norm(Axis);
  HingePoints[1,Number]:=Vector3TermMatrixMul(Vector3Add(HingePoint,Vector3ScalarMul(HingeAxis[Number],JOINT_DIST)),HingeiTransforms[Number]);
 end;
end;

procedure PhysicsJointHingeSetAngularVelocity(var Instance:TPhysicsJoint;Number:integer;Velocity:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  RigidBodies[Number]^.AngularVelocity:=Vector3TermMatrixMul(Vector3TermMatrixMul(Vector3ScalarMul(HingeAxis[Number],Velocity),RigidBodies[Number]^.Orientation),RigidBodies[Number]^.InvWorldInertiaTensor);
 end;
end;

function PhysicsJointResponseHinge(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    p,r,v:array[0..1] of TPhysicsVector3;
    Normal,Vel:TPhysicsVector3;
    NormalVel,ImpulseNumerator,ImpulseDenominator:TPhysicsFloat;
begin
 with Instance do begin
  for i:=0 to 1 do begin
   p[0]:=Vector3TermMatrixMul(HingePoints[i,0],RigidBodies[0]^.Transform);
   p[1]:=Vector3TermMatrixMul(HingePoints[i,1],RigidBodies[1]^.Transform);

   r[0]:=Vector3Sub(p[0],RigidBodies[0]^.Position);
   r[1]:=Vector3Sub(p[1],RigidBodies[1]^.Position);

   v[0]:=Vector3Add(Vector3Cross(RigidBodies[0]^.AngularVelocity,r[0]),RigidBodies[0]^.Velocity);
   v[1]:=Vector3Add(Vector3Cross(RigidBodies[1]^.AngularVelocity,r[1]),RigidBodies[1]^.Velocity);

   Vel:=Vector3Add(Vector3ScalarMul(Vector3Sub(p[0],p[1]),PhysicsInstance^.PenetrationSpeed/TimeToWork),Vector3Sub(v[0],v[1]));

   NormalVel:=Vector3Length(Vel);
   if NormalVel<EPSILON then begin
    continue;
   end else if NormalVel>PhysicsInstance^.VelocityMax then begin
    NormalVel:=PhysicsInstance^.VelocityMax;
   end;

   if HaveDestroyMinVelocity then begin
    if NormalVel>DestroyMinVelocity then begin
     result:=true;
     Active:=false;
     exit;
    end;
   end;

   Normal:=Vector3Norm(Vel);

   ImpulseNumerator:=-NormalVel;
   ImpulseDenominator:=RigidBodies[0]^.InvMass+
                       RigidBodies[1]^.InvMass+
                       Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[0],Normal),RigidBodies[0]^.InvWorldInertiaTensor),r[0]))+
                       Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[1],Normal),RigidBodies[1]^.InvWorldInertiaTensor),r[1]));

   if ImpulseDenominator<EPSILON then begin
    result:=false;
    exit;
   end;

   if not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
   end;
   if not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
   end;

   if (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) and not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
   end;
   if (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) and not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
   end;
  end;

  PhysicsJointRestrictionResponse(Instance,TimeToWork,RestrictionPoints[0],RestrictionPoints[1],RestrictionMinDist);

  result:=false;
 end;
end;

//////////////////////////////// Joint Universal ///////////////////////////////

procedure PhysicsJointInitUniversal(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;const Point,Axis0,Axis1,RestrictionAxis0,RestrictionAxis1:TPhysicsVector3;RestrictionAngle:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Rotation:TPhysicsMatrix4x4;
begin
 PhysicsJointInit(Instance,RigidBody0,RigidBody1);
 with Instance do begin
  JointType:=JointUniversal;

  UniversalPoint:=Point;

  UniversalAxis[0]:=Vector3Norm(Axis0);
  UniversalAxis[1]:=Vector3Norm(Axis1);

  UniversalPoints[0,0]:=Vector3TermMatrixMul(Point,RigidBody0^.InvTransform);
  UniversalPoints[0,1]:=Vector3TermMatrixMul(Point,RigidBody1^.InvTransform);

  UniversalPoints[1,0]:=Vector3TermMatrixMul(Vector3Add(Point,Vector3ScalarMul(UniversalAxis[0],JOINT_DIST)),RigidBody0^.InvTransform);
  UniversalPoints[1,1]:=Vector3TermMatrixMul(Vector3Add(Point,Vector3ScalarMul(UniversalAxis[0],JOINT_DIST)),RigidBody1^.InvTransform);

  Rotation:=Matrix4x4Rotation(RigidBody0^.InvTransform);
  Vector3MatrixMul(UniversalAxis[0],Rotation);
  Vector3MatrixMul(UniversalAxis[1],Rotation);

  RestrictionPoints[0]:=Vector3TermMatrixMul(Vector3Add(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis0),JOINT_DIST)),RigidBody0^.InvTransform);
  RestrictionPoints[1]:=Vector3TermMatrixMul(Vector3Sub(Point,Vector3ScalarMul(Vector3Norm(RestrictionAxis1),JOINT_DIST)),RigidBody1^.InvTransform);

  RestrictionMinDist:=sqrt(2*sqr(JOINT_DIST)*(1-cos(RestrictionAngle*DEG2RAD)));
 end;
end;

function PhysicsJointResponseUniversal(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    p,r,v,b:array[0..1] of TPhysicsVector3;
    Normal,Vel,a,d:TPhysicsVector3;
   NormalVel,ImpulseNumerator,ImpulseDenominator:TPhysicsFloat;
begin
 with Instance do begin
  for i:=0 to 1 do begin
   p[0]:=Vector3TermMatrixMul(UniversalPoints[i,0],RigidBodies[0]^.Transform);
   p[1]:=Vector3TermMatrixMul(UniversalPoints[i,1],RigidBodies[1]^.Transform);

   r[0]:=Vector3Sub(p[0],RigidBodies[0]^.Position);
   r[1]:=Vector3Sub(p[1],RigidBodies[1]^.Position);

   v[0]:=Vector3Add(Vector3Cross(RigidBodies[0]^.AngularVelocity,r[0]),RigidBodies[0]^.Velocity);
   v[1]:=Vector3Add(Vector3Cross(RigidBodies[1]^.AngularVelocity,r[1]),RigidBodies[1]^.Velocity);

   Vel:=Vector3Add(Vector3ScalarMul(Vector3Sub(p[0],p[1]),PhysicsInstance^.PenetrationSpeed/TimeToWork),Vector3Sub(v[0],v[1]));

   if i=1 then begin
    b[0]:=Vector3TermMatrixMul(UniversalAxis[1],Matrix4x4Rotation(RigidBodies[0]^.InvTransform));
    b[1]:=Vector3TermMatrixMul(UniversalAxis[1],Matrix4x4Rotation(RigidBodies[1]^.InvTransform));

    a:=Vector3Norm(Vector3Add(b[0],b[1]));

    d:=Vector3Mul(a,Vector3Mul(Vector3Sub(p[0],p[1]),a));

    Vel:=Vector3Add(Vector3ScalarMul(d,PhysicsInstance^.PenetrationSpeed/TimeToWork),Vector3Mul(a,Vector3Mul(Vector3Sub(v[0],v[1]),a)));
   end;

   NormalVel:=Vector3Length(Vel);
   if NormalVel<EPSILON then begin
    continue;
   end else if NormalVel>PhysicsInstance^.VelocityMax then begin
    NormalVel:=PhysicsInstance^.VelocityMax;
   end;

   if HaveDestroyMinVelocity then begin
    if NormalVel>DestroyMinVelocity then begin
     result:=true;
     Active:=false;
     exit;
    end;
   end;

   Normal:=Vector3Norm(Vel);

   ImpulseNumerator:=-NormalVel;
   ImpulseDenominator:=RigidBodies[0]^.InvMass+
                       RigidBodies[1]^.InvMass+
                       Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[0],Normal),RigidBodies[0]^.InvWorldInertiaTensor),r[0]))+
                       Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r[1],Normal),RigidBodies[1]^.InvWorldInertiaTensor),r[1]));

   if ImpulseDenominator<EPSILON then begin
    result:=false;
    exit;
   end;
   
   if not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
   end;
   if not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
   end;

   if (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) and not (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[1]^,p[1],Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
   end;
   if (RigidBodies[1]^.Immovable or RigidBodies[1]^.Static) and not (RigidBodies[0]^.Immovable or RigidBodies[0]^.Static) then begin
    PhysicsRigidBodyAddImpulse(RigidBodies[0]^,p[0],Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
   end;
  end;

  PhysicsJointRestrictionResponse(Instance,TimeToWork,RestrictionPoints[0],RestrictionPoints[1],RestrictionMinDist);

  result:=false;
 end;
end;

/////////////////////////////////// Joint User /////////////////////////////////

procedure PhysicsJointInitUser(var Instance:TPhysicsJoint;RigidBody0,RigidBody1:PPhysicsRigidBody;UserResponseProc:TPhysicsJointUserResponseProc;Data:pointer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsJointInit(Instance,RigidBody0,RigidBody1);
 Instance.UserResponseProc:=UserResponseProc;
 Instance.Data:=Data;
end;

function PhysicsJointResponseUser(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if not Instance.Active then begin
  result:=true;
  exit;
 end;
 result:=Instance.UserResponseProc(@Instance,TimeToWork,Instance.Data);
end;

////////////////////////////////////// Joint ///////////////////////////////////

const PhysicsJointResponseProcs:TPhysicsJointResponseProcs=(PhysicsJointResponseBase,
                                                            PhysicsJointResponseBall,
                                                            PhysicsJointResponseHinge,
                                                            PhysicsJointResponseUniversal,
                                                            PhysicsJointResponseUser);

function PhysicsJointResponse(var Instance:TPhysicsJoint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if not (Instance.Active and (Instance.JointType in [JointStart..JointEnd])) then begin
  result:=true;
  exit;
 end;
 result:=PhysicsJointResponseProcs[Instance.JointType](Instance,TimeToWork);
end;

//////////////////////////////////// Constraint ////////////////////////////////

procedure PhysicsConstraintInit(var Instance:TPhysicsConstraint;RigidBody1:PPhysicsRigidBody;RigidBody2:PPhysicsRigidBody=nil); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j:integer;
begin
 fillchar(Instance,sizeof(TPhysicsConstraint),#0);
 Instance.RigidBodies[0]:=RigidBody1;
 Instance.RigidBodies[1]:=RigidBody2;
 Instance.ConstraintType:=ConstraintNone;
 with Instance do begin
  for i:=0 to 1 do begin
   if assigned(RigidBodies[i]) then begin
    inc(RigidBodies[i]^.NumConstraints);
    j:=(RigidBodies[i]^.NumConstraints+MemoryInc) and not MemoryIncMask;
    if j<>RigidBodies[i]^.AllConstraints then begin
     RigidBodies[i]^.AllConstraints:=j;
     PhysicsReallocateMemory(RigidBodies[i]^.Constraints,RigidBodies[i]^.AllConstraints*sizeof(PPhysicsConstraint));
    end;
    RigidBodies[i]^.Constraints^[RigidBodies[i]^.NumConstraints-1]:=@Instance;
   end;
  end;
 end;
 inc(PhysicsInstance^.NumConstraints);
 i:=(PhysicsInstance^.NumConstraints+MemoryInc) and not MemoryIncMask;
 if i<>PhysicsInstance^.AllConstraints then begin
  PhysicsInstance^.AllConstraints:=i;
  PhysicsReallocateMemory(PhysicsInstance^.Constraints,PhysicsInstance^.AllConstraints*sizeof(PPhysicsConstraint));
 end;
 PhysicsInstance^.Constraints^[PhysicsInstance^.NumConstraints-1]:=@Instance;
end;

procedure PhysicsConstraintDone(var Instance:TPhysicsConstraint); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j,k:integer;
begin
 with Instance do begin
  for i:=0 to 1 do begin
   if assigned(RigidBodies[i]) then begin
    k:=-1;
    for j:=0 to RigidBodies[i]^.NumConstraints-1 do begin
     if RigidBodies[i]^.Constraints[j]=@Instance then begin
      k:=j;
      break;
     end;
    end;
    if k>=0 then begin
     dec(RigidBodies[i]^.NumConstraints);
     move(RigidBodies[i]^.Constraints[k+1],RigidBodies[i]^.Constraints[k],RigidBodies[i]^.NumConstraints*sizeof(PPhysicsConstraint));
     j:=(RigidBodies[i]^.NumConstraints+MemoryInc) and not MemoryIncMask;
     if j<>RigidBodies[i]^.AllConstraints then begin
      RigidBodies[i]^.AllConstraints:=j;
      PhysicsReallocateMemory(RigidBodies[i]^.Constraints,RigidBodies[i]^.AllConstraints*sizeof(PPhysicsConstraint));
     end;
    end;
   end;
  end;

  j:=-1;
  for i:=0 to PhysicsInstance^.NumConstraints-1 do begin
   if PhysicsInstance^.Constraints[i]=@Instance then begin
    j:=i;
    break;
   end;
  end;
  if j>=0 then begin
   dec(PhysicsInstance^.NumConstraints);
   move(PhysicsInstance^.Constraints[j+1],PhysicsInstance^.Constraints[j],PhysicsInstance^.NumConstraints*sizeof(PPhysicsConstraint));
   i:=(PhysicsInstance^.NumConstraints+MemoryInc) and not MemoryIncMask;
   if i<>PhysicsInstance^.AllConstraints then begin
    PhysicsInstance^.AllConstraints:=i;
    PhysicsReallocateMemory(PhysicsInstance^.Constraints,PhysicsInstance^.AllConstraints*sizeof(PPhysicsConstraint));
   end;
  end;

 end;
 fillchar(Instance,sizeof(TPhysicsConstraint),#0);
end;

procedure PhysicsConstraintSetActive(var Instance:TPhysicsConstraint;Active:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Instance.Active:=Active;
end;

function PhysicsConstraintIsActive(var Instance:TPhysicsConstraint):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.Active;
end;

/////////////////////////////// Constraint Max Distance ////////////////////////

procedure PhysicsConstraintInitMaxDistance(var Instance:TPhysicsConstraint;RigidBody1,RigidBody2:PPhysicsRigidBody;MaxDistance:TPhysicsFloat;MaxDistanceBodyPos0,MaxDistanceBodyPos1:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsConstraintInit(Instance,RigidBody1,RigidBody2);
 Instance.ConstraintType:=ConstraintMaxDistance;
 Instance.MaxDistance:=MaxDistance;
 Instance.MaxDistanceBodyPos0:=MaxDistanceBodyPos0;
 Instance.MaxDistanceBodyPos1:=MaxDistanceBodyPos1;
end;

function PhysicsConstraintResponseMaxDistance(var Instance:TPhysicsConstraint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var r0,r1,wp0,wp1,wp,CurrentRelPos,CurrentVel0,CurrentVel1,PredRelPos,
    ClampedRelPos,DesiredRelVel,Vr,Normal:TPhysicsVector3;
    ClampedRelPosMag,NormalVel,ImpulseNumerator,ImpulseDenominator:TPhysicsFloat;
begin
 if (Instance.RigidBodies[0]^.Static or Instance.RigidBodies[0]^.Frozen) and
    (Instance.RigidBodies[1]^.Static or Instance.RigidBodies[1]^.Frozen) then begin
  result:=false;
  exit;
 end;

 r0:=Vector3TermMatrixMul(Instance.MaxDistanceBodyPos0,Instance.RigidBodies[0]^.Orientation);
 r1:=Vector3TermMatrixMul(Instance.MaxDistanceBodyPos1,Instance.RigidBodies[1]^.Orientation);

 wp0:=Vector3Add(Instance.RigidBodies[0]^.Position,r0);
 wp1:=Vector3Add(Instance.RigidBodies[1]^.Position,r1);
 wp:=Vector3ScalarMul(Vector3Add(wp0,wp1),0.5);
 CurrentRelPos:=Vector3Sub(wp0,wp1);

 CurrentVel0:=Vector3Add(Instance.RigidBodies[0]^.Velocity,Vector3Cross(Instance.RigidBodies[0]^.AngularVelocity,r0));
 CurrentVel1:=Vector3Add(Instance.RigidBodies[1]^.Velocity,Vector3Cross(Instance.RigidBodies[1]^.AngularVelocity,r1));

 PredRelPos:=Vector3Add(CurrentRelPos,Vector3ScalarMul(Vector3Sub(CurrentVel0,CurrentVel1),TimeToWork));

 ClampedRelPos:=PredRelPos;
 ClampedRelPosMag:=Vector3Length(ClampedRelPos);
 if ClampedRelPosMag<=EPSILON then begin
  result:=false;
  exit;
 end else if ClampedRelPosMag>Instance.MaxDistance then begin
  ClampedRelPos:=Vector3ScalarMul(ClampedRelPos,Instance.MaxDistance/ClampedRelPosMag);
 end;

 DesiredRelVel:=Vector3ScalarMul(Vector3Sub(ClampedRelPos,CurrentRelPos),1/Max(TimeToWork,EPSILON));

 Vr:=Vector3Sub(Vector3Sub(CurrentVel0,CurrentVel1),DesiredRelVel);

 NormalVel:=Vector3Length(Vr);
 if NormalVel>PhysicsInstance^.VelocityMax then begin
  Vr:=Vector3ScalarMul(Vr,PhysicsInstance^.VelocityMax/NormalVel);
  NormalVel:=PhysicsInstance^.VelocityMax;
 end else if NormalVel<PhysicsInstance^.VelocityThreshold then begin
  result:=false;
  exit;
 end;

 Normal:=Vector3ScalarMul(Vr,1/NormalVel);

 ImpulseNumerator:=-NormalVel;
 ImpulseDenominator:=Instance.RigidBodies[0]^.InvMass+
                     Instance.RigidBodies[1]^.InvMass+
                     Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r0,Normal),Instance.RigidBodies[0]^.InvWorldInertiaTensor),r0))+
                     Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r1,Normal),Instance.RigidBodies[1]^.InvWorldInertiaTensor),r1));

 if ImpulseDenominator<EPSILON then begin
  result:=false;
  exit;
 end;

 if not (Instance.RigidBodies[0]^.Immovable or Instance.RigidBodies[0]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[0]^,wp,Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
 end;
 if not (Instance.RigidBodies[1]^.Immovable or Instance.RigidBodies[1]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[1]^,wp,Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
 end;

 if (Instance.RigidBodies[0]^.Immovable or Instance.RigidBodies[0]^.Static) and not (Instance.RigidBodies[1]^.Immovable or Instance.RigidBodies[1]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[1]^,wp,Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
 end;
 if (Instance.RigidBodies[1]^.Immovable or Instance.RigidBodies[1]^.Static) and not (Instance.RigidBodies[0]^.Immovable or Instance.RigidBodies[0]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[0]^,wp,Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
 end;                       

 result:=true;
end;

////////////////////////////////// Constraint Point ////////////////////////////

procedure PhysicsConstraintInitPoint(var Instance:TPhysicsConstraint;RigidBody1,RigidBody2:PPhysicsRigidBody;PointAllowedDistance,PointTimeScale:TPhysicsFloat;PointBodyPos0,PointBodyPos1:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsConstraintInit(Instance,RigidBody1,RigidBody2);
 Instance.ConstraintType:=ConstraintPoint;
 Instance.PointAllowedDistance:=PointAllowedDistance;
 Instance.PointTimeScale:=Max(PointTimeScale,EPSILON);
 Instance.PointBodyPos0:=PointBodyPos0;
 Instance.PointBodyPos1:=PointBodyPos1;
end;

function PhysicsConstraintResponsePoint(var Instance:TPhysicsConstraint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var r0,r1,wp0,wp1,wp,Deviation,VrExtra,CurrentVel0,CurrentVel1,Vr,
    Normal:TPhysicsVector3;
    DeviationAmount,NormalVel,ImpulseNumerator,ImpulseDenominator:TPhysicsFloat;
begin
 if (Instance.RigidBodies[0]^.Static or Instance.RigidBodies[0]^.Frozen) and
    (Instance.RigidBodies[1]^.Static or Instance.RigidBodies[1]^.Frozen) then begin
  result:=false;
  exit;
 end;

 r0:=Vector3TermMatrixMul(Instance.PointBodyPos0,Instance.RigidBodies[0]^.Orientation);
 r1:=Vector3TermMatrixMul(Instance.PointBodyPos1,Instance.RigidBodies[1]^.Orientation);

 wp0:=Vector3Add(Instance.RigidBodies[0]^.Position,r0);
 wp1:=Vector3Add(Instance.RigidBodies[1]^.Position,r1);
 wp:=Vector3ScalarMul(Vector3Add(wp0,wp1),0.5);

 Deviation:=Vector3Sub(wp0,wp1);
 DeviationAmount:=Vector3Length(Deviation);
 if DeviationAmount>Instance.PointAllowedDistance then begin
  VrExtra:=Vector3ScalarMul(Deviation,(DeviationAmount-Instance.PointAllowedDistance)/(DeviationAmount*Max(TimeToWork,Instance.PointTimeScale)));
 end else begin
  VrExtra:=Vector3Origin;
 end;

 CurrentVel0:=Vector3Add(Instance.RigidBodies[0]^.Velocity,Vector3Cross(Instance.RigidBodies[0]^.AngularVelocity,r0));
 CurrentVel1:=Vector3Add(Instance.RigidBodies[1]^.Velocity,Vector3Cross(Instance.RigidBodies[1]^.AngularVelocity,r1));

 Vr:=Vector3Add(VrExtra,Vector3Sub(CurrentVel0,CurrentVel1));

 NormalVel:=Vector3Length(Vr);
 if NormalVel>PhysicsInstance^.VelocityMax then begin
  Vr:=Vector3ScalarMul(Vr,PhysicsInstance^.VelocityMax/NormalVel);
  NormalVel:=PhysicsInstance^.VelocityMax;
 end else if NormalVel<PhysicsInstance^.VelocityThreshold then begin
  result:=false;
  exit;
 end;

 Normal:=Vector3ScalarMul(Vr,1/NormalVel);

 ImpulseNumerator:=-NormalVel;
 ImpulseDenominator:=Instance.RigidBodies[0]^.InvMass+
                     Instance.RigidBodies[1]^.InvMass+
                     Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r0,Normal),Instance.RigidBodies[0]^.InvWorldInertiaTensor),r0))+
                     Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r1,Normal),Instance.RigidBodies[1]^.InvWorldInertiaTensor),r1));

 if ImpulseDenominator<EPSILON then begin
  result:=false;
  exit;
 end;

 if not (Instance.RigidBodies[0]^.Immovable or Instance.RigidBodies[0]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[0]^,wp,Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
 end;
 if not (Instance.RigidBodies[1]^.Immovable or Instance.RigidBodies[1]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[1]^,wp,Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
 end;

 if (Instance.RigidBodies[0]^.Immovable or Instance.RigidBodies[0]^.Static) and not (Instance.RigidBodies[1]^.Immovable or Instance.RigidBodies[1]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[1]^,wp,Vector3Neg(Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator)));
 end;
 if (Instance.RigidBodies[1]^.Immovable or Instance.RigidBodies[1]^.Static) and not (Instance.RigidBodies[0]^.Immovable or Instance.RigidBodies[0]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[0]^,wp,Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
 end;

 result:=true;
end;

//////////////////////////////// Constraint World Point ////////////////////////

procedure PhysicsConstraintInitWorldPoint(var Instance:TPhysicsConstraint;RigidBody:PPhysicsRigidBody;WorldPointPointOnBody,WorldPoint:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsConstraintInit(Instance,RigidBody);
 Instance.ConstraintType:=ConstraintWorldPoint;
 Instance.WorldPointPointOnBody:=WorldPointPointOnBody;
 Instance.WorldPoint:=WorldPoint;
end;

function PhysicsConstraintResponseWorldPoint(var Instance:TPhysicsConstraint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const AllowedDeviation=0.01;
var r,WorldPos,CurrentVel,DesiredVel,Deviation,DeviationDir,
    Normal:TPhysicsVector3;
    TimeScale,DeviationDistance,NormalVel,ImpulseNumerator,
    ImpulseDenominator:TPhysicsFloat;
begin
 if Instance.RigidBodies[0]^.Static or Instance.RigidBodies[0]^.Frozen then begin
  result:=false;
  exit;
 end;

 TimeScale:=TimeToWork*4.0;

 r:=Vector3TermMatrixMul(Instance.WorldPointPointOnBody,Instance.RigidBodies[0]^.Orientation);
 WorldPos:=Vector3Add(Instance.RigidBodies[0]^.Position,r);
 CurrentVel:=Vector3Add(Instance.RigidBodies[0]^.Velocity,Vector3Cross(Instance.RigidBodies[0]^.AngularVelocity,r));

 Deviation:=Vector3Sub(WorldPos,Instance.WorldPoint);
 DeviationDistance:=Vector3Length(Deviation);
 if DeviationDistance>AllowedDeviation then begin
  DeviationDir:=Vector3ScalarMul(Deviation,1/DeviationDistance);
  DesiredVel:=Vector3ScalarMul(DeviationDir,(AllowedDeviation-DeviationDistance)/TimeScale);
 end else begin
  DesiredVel:=Vector3Origin;
 end;

 Normal:=Vector3Sub(CurrentVel,DesiredVel);
 NormalVel:=Vector3Length(Normal);
 if NormalVel<PhysicsInstance^.VelocityThreshold then begin
  result:=false;
  exit;
 end;

 Normal:=Vector3ScalarMul(Normal,1/NormalVel);

 ImpulseNumerator:=-NormalVel;
 ImpulseDenominator:=Instance.RigidBodies[0]^.InvMass+
                     Vector3Dot(Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r,Normal),Instance.RigidBodies[0]^.InvWorldInertiaTensor),r));

 if ImpulseDenominator<EPSILON then begin
  result:=false;
  exit;
 end;

 if not (Instance.RigidBodies[0]^.Immovable or Instance.RigidBodies[0]^.Static) then begin
  PhysicsRigidBodyAddImpulse(Instance.RigidBodies[0]^,WorldPos,Vector3ScalarMul(Normal,ImpulseNumerator/ImpulseDenominator));
 end;

 result:=false;
end;

////////////////////////////////// Constraint Velocity /////////////////////////

procedure PhysicsConstraintInitVelocity(var Instance:TPhysicsConstraint;RigidBody:PPhysicsRigidBody;Velocity,VelocityAngular:TPhysicsVector3;VelocityDo,VelocityDoAngular:boolean); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsConstraintInit(Instance,RigidBody);
 Instance.ConstraintType:=ConstraintVelocity;
 Instance.Velocity:=Velocity;
 Instance.VelocityAngular:=VelocityAngular;
 Instance.VelocityDo:=VelocityDo;
 Instance.VelocityDoAngular:=VelocityDoAngular;
end;

function PhysicsConstraintResponseVelocity(var Instance:TPhysicsConstraint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const SmoothTime=0.2;
 procedure Smooth(var Val,ValRate:TPhysicsVector3;ToVal:TPhysicsVector3);
 var Omega,x,expfactor:TPhysicsFloat;
     Change,Temp:TPhysicsVector3;
 begin
  if SmoothTime>EPSILON then begin
   Omega:=2.0/SmoothTime;
   x:=Omega*TimeToWork;
   expfactor:=1.0/(1.0+x+(0.48*x*x)+(0.235*x*x*x));
   Change:=Vector3Sub(Val,ToVal);
   Temp:=Vector3ScalarMul(Vector3Add(ValRate,Vector3ScalarMul(Change,Omega)),TimeToWork);
   ValRate:=Vector3ScalarMul(Vector3Sub(ValRate,Vector3ScalarMul(Temp,Omega)),expfactor);
   Val:=Vector3Add(ToVal,Vector3ScalarMul(Vector3Add(Change,Temp),expfactor));
  end else if TimeToWork>EPSILON then begin
   ValRate:=Vector3ScalarMul(Vector3Sub(ToVal,Val),1/TimeToWork);
   Val:=ToVal;
  end else begin
   ValRate:=Vector3Origin;
   Val:=ToVal;
  end;
 end;
var v:TPhysicsVector3;
begin
 if Instance.RigidBodies[0]^.Static or Instance.RigidBodies[0]^.Frozen then begin
  result:=false;
  exit;
 end;
 result:=false;
 if Instance.VelocityDo then begin
  v:=Instance.RigidBodies[0]^.Velocity;
  Smooth(v,Instance.VelocityCurrentRate,Instance.Velocity);
  Instance.RigidBodies[0]^.Velocity:=Vector3Add(Vector3ScalarMul(Vector3TermMatrixMul(v,Instance.RigidBodies[0]^.Orientation),0.5),Vector3ScalarMul(Instance.RigidBodies[0]^.Velocity,0.5));
  result:=true;
 end;
 if Instance.VelocityDoAngular then begin
  v:=Instance.RigidBodies[0]^.AngularVelocity;
  Smooth(v,Instance.VelocityAngularCurrentRate,Instance.VelocityAngular);
  Instance.RigidBodies[0]^.AngularVelocity:=Vector3Add(Vector3ScalarMul(Vector3TermMatrixMul(v,Instance.RigidBodies[0]^.Orientation),0.5),Vector3ScalarMul(Instance.RigidBodies[0]^.AngularVelocity,0.5));
  result:=true;
 end;
end;

//////////////////////////////////// Constraint User ///////////////////////////

procedure PhysicsConstraintInitUser(var Instance:TPhysicsConstraint;RigidBody:PPhysicsRigidBody;UserResponseProc:TPhysicsConstraintUserResponseProc;Data:pointer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsConstraintInit(Instance,RigidBody);
 Instance.ConstraintType:=ConstraintUser;
 Instance.UserResponseProc:=UserResponseProc;
 Instance.Data:=Data;
end;

function PhysicsConstraintResponseUser(var Instance:TPhysicsConstraint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 result:=Instance.UserResponseProc(@Instance,TimeToWork,Instance.Data);
end;

const PhysicsConstraintResponseProcs:TPhysicsConstraintResponseProcs=(PhysicsConstraintResponseMaxDistance,
                                                                      PhysicsConstraintResponsePoint,
                                                                      PhysicsConstraintResponseWorldPoint,
                                                                      PhysicsConstraintResponseVelocity,
                                                                      PhysicsConstraintResponseUser);

function PhysicsConstraintResponse(var Instance:TPhysicsConstraint;TimeToWork:TPhysicsFloat):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 if not (Instance.Active and (Instance.ConstraintType in [ConstraintStart..ConstraintEnd])) then begin
  result:=true;
  exit;
 end;
 result:=PhysicsConstraintResponseProcs[Instance.ConstraintType](Instance,TimeToWork);
end;

//////////////////////////////////// Rigid Body ////////////////////////////////

procedure PhysicsRigidBodyInit(var Instance:TPhysicsRigidBody;AObject:PPhysicsObject;AMass,ARestitution,AFriction:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Min,Max,v:TPhysicsVector3;
    Radius,Length,CylinderMass,EndMass:TPhysicsFloat;
begin
 fillchar(Instance,sizeof(TPhysicsRigidBody),#0);
 with Instance do begin
  ID:=PhysicsInstance^.RigidBodyID;
  inc(PhysicsInstance^.RigidBodyID);

  OwnerObject:=AObject;

  Mass:=AMass;
  if Mass<EPSILON then begin
   Mass:=EPSILON;
  end;
  InvMass:=1/Mass;
  
  Restitution:=ARestitution;
  Friction:=AFriction;

  OwnerObject^.RigidBody:=@Instance;

  New(Collide);
  PhysicsCollideInit(Collide^);

  Volume:=0;
  CenterOfMass:=Vector3Origin;
  InertiaTensor:=Matrix3x3Identity;

  PhysicsRigidBodyCalculateMeshVolume(Instance);

  case OwnerObject^.BodyType of
   BodyMesh:begin
    PhysicsRigidBodyCalculateMeshCenterOfMass(Instance);
    PhysicsRigidBodyCalculateMeshInertiaTensor(Instance);
//  PhysicsObjectTranslate(OwnerObject^,Vector3Neg(CenterOfMass));
    PhysicsObjectFinish(OwnerObject^);       
    Matrix3x3Inverse(InvBodyInertiaTensor,InertiaTensor);
   end;
   BodyBox:begin
    Min:=OwnerObject^.AABB.Min;
    Max:=OwnerObject^.AABB.Max;
    Volume:=(Max.x-Min.x)*(Max.y-Min.y)*(Max.z-Min.z);
    v:=Vector3ScalarMul(Vector3Sub(Max,Min),0.5);
    InertiaTensor[0,0]:=(1/12)*(Mass*(sqr(v.y)*sqr(v.z)));
    InertiaTensor[1,1]:=(1/12)*(Mass*(sqr(v.x)*sqr(v.z)));
    InertiaTensor[2,2]:=(1/12)*(Mass*(sqr(v.x)*sqr(v.y)));
    Matrix3x3Inverse(InvBodyInertiaTensor,InertiaTensor);
   end;
   BodySphere:begin
    Radius:=OwnerObject^.Sphere.Radius;
    Volume:=((Radius*Radius*Radius)*pi)*(4/3);
    InertiaTensor[0,0]:=(2/5)*(Mass*sqr(Radius));
    InertiaTensor[1,1]:=(2/5)*(Mass*sqr(Radius));
    InertiaTensor[2,2]:=(2/5)*(Mass*sqr(Radius));
    Matrix3x3Inverse(InvBodyInertiaTensor,InertiaTensor);
   end;
   BodyCylinder:begin
    Min:=OwnerObject^.AABB.Min;
    Max:=OwnerObject^.AABB.Max;
    Radius:=Max.x-Min.x;
    Length:=Max.y-Min.y;
    Volume:=sqr(Radius)*Length;
    InertiaTensor[0,0]:=(1/12)*(Mass*sqr(Length));
    InertiaTensor[1,1]:=(1/12)*(Mass*sqr(Length));
    InertiaTensor[2,2]:=(1/2)*(Mass*sqr(Radius));
    Matrix3x3Inverse(InvBodyInertiaTensor,InertiaTensor);
   end;
   BodyCapsule:begin
    Min:=OwnerObject^.AABB.Min;
    Max:=OwnerObject^.AABB.Max;
    Radius:=MaxEx(abs(Max.x-Min.x),abs(Max.y-Min.y))*0.5;
    Length:=abs(Max.z-Min.z)-(Radius*2);
    Volume:=((4/3)*pi*Radius*Radius*Radius)+(Length*pi*sqr(Radius));
    CylinderMass:=(Mass*pi*sqr(Radius)*Length)/Volume;
    EndMass:=Mass-CylinderMass;
    InertiaTensor[0,0]:=((1/12)*(CylinderMass*sqr(Length)))+(0.25*CylinderMass*sqr(Radius))+(0.4*EndMass*sqr(Radius))+(EndMass*sqr(0.5*Length));
    InertiaTensor[1,1]:=((1/12)*(CylinderMass*sqr(Length)))+(0.25*CylinderMass*sqr(Radius))+(0.4*EndMass*sqr(Radius))+(EndMass*sqr(0.5*Length));
    InertiaTensor[2,2]:=((1/2)*(CylinderMass*sqr(Radius)))+(0.2*EndMass*sqr(Radius));
    Matrix3x3Inverse(InvBodyInertiaTensor,InertiaTensor);
   end;
  end;

  VelocityDamp:=0.0;
  AngularVelocityDamp:=0.0;
  AdditionalDamping:=false;
  AdditionalDamp:=0.005;
  VelocityAdditionalDamp:=0.01;
  AngularVelocityAdditionalDamp:=0.01;
  VelocityAdditionalDampThresholdSqr:=0.01;
  AngularVelocityAdditionalDampThresholdSqr:=0.01;

  DoGravitation:=true;

  Frozen:=false;
  Immovable:=false;
  Static:=false;

  PhysicsRigidBodySetMatrix(Instance,OwnerObject^.Transform);

  OldTransform:=Transform;
  OldInvTransform:=InvTransform;

  inc(PhysicsInstance^.NumRigidBodies);
  if PhysicsInstance^.NumRigidBodies>PhysicsInstance^.AllRigidBodies then begin
   PhysicsInstance^.AllRigidBodies:=(PhysicsInstance^.NumRigidBodies+MemoryInc) and not MemoryIncMask;
   PhysicsReallocateMemory(PhysicsInstance^.RigidBodies,PhysicsInstance^.AllRigidBodies*sizeof(PPhysicsRigidBody));
  end;
  PhysicsInstance^.RigidBodies^[PhysicsInstance^.NumRigidBodies-1]:=@Instance;
 end;
end;

procedure PhysicsRigidBodyDone(var Instance:TPhysicsRigidBody);
var i,j:integer;
begin
 with Instance do begin
  while NumJoints>0 do begin
   PhysicsJointDone(Joints^[0]^);
  end;
  if assigned(Joints) then begin
   freemem(Joints);
   Joints:=nil;
   NumJoints:=0;
  end;

  while NumConstraints>0 do begin
   PhysicsConstraintDone(Constraints^[0]^);
  end;
  if assigned(Constraints) then begin
   freemem(Constraints);
   Constraints:=nil;
   NumConstraints:=0;
  end;
 end;
 j:=-1;
 for i:=0 to PhysicsInstance^.NumRigidBodies-1 do begin
  if PhysicsInstance^.RigidBodies^[i]=@Instance then begin
   j:=i;
   break;
  end;
 end;
 if j>=0 then begin
  move(PhysicsInstance^.RigidBodies^[j+1],PhysicsInstance^.RigidBodies^[j],(PhysicsInstance^.NumRigidBodies-j)*sizeof(PPhysicsRigidBody));
  dec(PhysicsInstance^.NumRigidBodies);
  i:=(PhysicsInstance^.NumRigidBodies+MemoryInc) and not MemoryIncMask;
  if i<>PhysicsInstance^.AllRigidBodies then begin
   PhysicsInstance^.AllRigidBodies:=i;
   PhysicsReallocateMemory(PhysicsInstance^.RigidBodies,PhysicsInstance^.AllRigidBodies*sizeof(PPhysicsRigidBody));
  end;
 end;
 PhysicsCollideDone(Instance.Collide^);
 dispose(Instance.Collide);
end;

procedure PhysicsRigidBodyCalculateMeshVolume(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    v:TPhysicsFloat;
 procedure ProcessMesh(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  for i:=0 to Mesh.NumMeshs-1 do begin
   ProcessMesh(Mesh.Meshs^[i]^);
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   v:=v+Matrix3x3Determinant(Matrix3x3(Mesh.Triangles^[i].Vertices[0],Mesh.Triangles^[i].Vertices[1],Mesh.Triangles^[i].Vertices[2]));
  end;
 end;
begin
 v:=0;
 if assigned(Instance.OwnerObject) then begin
  for i:=0 to Instance.OwnerObject^.NumMeshs-1 do begin
   ProcessMesh(Instance.OwnerObject^.Meshs^[i]^);
  end;
 end;
 Instance.Volume:=v/6;
end;

procedure PhysicsRigidBodyCalculateMeshCenterOfMass(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    v,lv:TPhysicsFloat;
    com:TPhysicsVector3;
    m:TPhysicsMatrix3x3;
 procedure ProcessMesh(var Mesh:TPhysicsObjectMesh);
 var i:integer;
 begin
  for i:=0 to Mesh.NumMeshs-1 do begin
   ProcessMesh(Mesh.Meshs^[i]^);
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   m:=Matrix3x3TermTranspose(Matrix3x3(Mesh.Triangles^[i].Vertices[0],
                                       Mesh.Triangles^[i].Vertices[1],
                                       Mesh.Triangles^[i].Vertices[2]));
   lv:=Matrix3x3Determinant(m);
   com:=Vector3Add(com,Vector3ScalarMul(Vector3Add(Vector3Add(Vector3(m[0,0],m[0,1],m[0,2]),
                                                              Vector3(m[1,0],m[1,1],m[1,2])),
                                                              Vector3(m[2,0],m[2,1],m[2,2])),lv));
   v:=v+lv;
  end;
 end;
begin
 v:=0;
 com:=Vector3Origin;
 if assigned(Instance.OwnerObject) then begin
  for i:=0 to Instance.OwnerObject^.NumMeshs-1 do begin
   ProcessMesh(Instance.OwnerObject^.Meshs^[i]^);
  end;
 end;
 if v<EPSILON then begin
  v:=EPSILON*2;
 end;
 Instance.CenterOfMass:=Vector3ScalarMul(com,1/(v*4));
end;

procedure PhysicsRigidBodyCalculateMeshInertiaTensor(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const nv:array[0..2] of integer=(1,2,0);
var i:integer;
    v:TPhysicsFloat;
    diag,offd:TPhysicsVector3;
    m:TPhysicsMatrix3x3;
 procedure ProcessMesh(var Mesh:TPhysicsObjectMesh);
 var i,j,j1,j2:integer;
     d:TPhysicsFloat;
 begin
  for i:=0 to Mesh.NumMeshs-1 do begin
   ProcessMesh(Mesh.Meshs^[i]^);
  end;
  for i:=0 to Mesh.NumTriangles-1 do begin
   m:=Matrix3x3(Vector3Sub(Mesh.Triangles^[i].Vertices[0],Instance.CenterOfMass),
                Vector3Sub(Mesh.Triangles^[i].Vertices[1],Instance.CenterOfMass),
                Vector3Sub(Mesh.Triangles^[i].Vertices[2],Instance.CenterOfMass));
   d:=Matrix3x3Determinant(Matrix3x3TermTranspose(m));
   v:=v+d;
   for j:=0 to 2 do begin
    j1:=nv[j];
    j2:=nv[j1];
    diag.xyz[j]:=diag.xyz[j]+(((m[0][j]*m[1][j])+(m[1][j]*m[2][j])+(m[2][j]*m[0][j])+
                               (m[0][j]*m[0][j])+(m[1][j]*m[1][j])+(m[2][j]*m[2][j]))*d);
    offd.xyz[j]:=offd.xyz[j]+(((m[0][j1]*m[1][j2])+(m[1][j1]*m[2][j2])+(m[2][j1]*m[0][j2])+
                               (m[0][j1]*m[2][j2])+(m[1][j1]*m[0][j2])+(m[2][j1]*m[1][j2])+
                               (m[0][j1]*m[0][j2]*2)+(m[1][j1]*m[1][j2]*2)+(m[2][j1]*m[2][j2]*2))*d);
   end;
  end;
 end;
begin
 diag:=Vector3Origin;
 offd:=Vector3Origin;
 v:=0;
 if assigned(Instance.OwnerObject) then begin
  for i:=0 to Instance.OwnerObject^.NumMeshs-1 do begin
   ProcessMesh(Instance.OwnerObject^.Meshs^[i]^);
  end;
 end;
 if v<EPSILON then begin
  v:=EPSILON*2;
 end;
 diag:=Vector3ScalarMul(diag,1/(v*(60/6)));
 offd:=Vector3ScalarMul(offd,1/(v*(120/6)));
 Instance.InertiaTensor:=Matrix3x3(diag.y+diag.z,-offd.z,-offd.y,
                                   -offd.z,diag.x+diag.z,-offd.x,
                                   -offd.y,-offd.x,diag.x+diag.y);
end;

procedure PhysicsRigidBodySetMatrix(var Instance:TPhysicsRigidBody;m:TPhysicsMatrix4x4); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  Position:=Vector3Origin;
  Vector3MatrixMul(Position,m);

  Orientation:=Matrix4x4GetSubMatrix3x3(m,0,0);

  OldTransform:=Transform;
  OldInvTransform:=InvTransform;
  Transform:=m;
  Matrix4x4Inverse(InvTransform,Transform);

  Velocity:=Vector3Origin;
  AngularVelocity:=Vector3Origin;

  InvWorldInertiaTensor:=Matrix3x3Identity;

  PhysicsObjectUpdatePosition(OwnerObject^,Position);

  Frozen:=false;
  FrozenTime:=0;
  Immovable:=true;
 end;
end;

procedure PhysicsRigidBodySetVector(var Instance:TPhysicsRigidBody;v:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var m:TPhysicsMatrix4x4;
begin
 m:=Matrix4x4Translate(v);
 PhysicsRigidBodySetMatrix(Instance,m);
end;

procedure PhysicsRigidBodyLimit(var Instance:TPhysicsRigidBody); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Vel:TPhysicsFloat;
begin
 with Instance do begin
  Vel:=Vector3Length(Velocity);
  if Vel>PhysicsInstance^.VelocityMax then begin
   Vector3Scale(Velocity,PhysicsInstance^.VelocityMax/Vel);
  end;
  Vel:=Vector3Length(AngularVelocity);
  if Vel>PhysicsInstance^.AngularVelocityMax then begin
   Vector3Scale(AngularVelocity,PhysicsInstance^.AngularVelocityMax/Vel);
  end;
 end;
end;

procedure PhysicsRigidBodySetAngularMomentum(var Instance:TPhysicsRigidBody;const AngularMomentum:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 Instance.AngularVelocity:=Vector3TermMatrixMul(AngularMomentum,Instance.InvWorldInertiaTensor);
 PhysicsRigidBodyLimit(Instance);
end;

function PhysicsRigidBodyGetAngularMomentum(var Instance:TPhysicsRigidBody):TPhysicsVector3; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var WorldInertiaTensor:TPhysicsMatrix3x3;
begin
 Matrix3x3Inverse(WorldInertiaTensor,Instance.InvWorldInertiaTensor);
 result:=Vector3TermMatrixMul(Instance.AngularVelocity,WorldInertiaTensor);
end;

procedure PhysicsRigidBodyAddImpulse(var Instance:TPhysicsRigidBody;const Point,Impulse:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 with Instance do begin
  Velocity:=Vector3Add(Velocity,Vector3ScalarMul(Impulse,InvMass));
  AngularVelocity:=Vector3Add(AngularVelocity,Vector3TermMatrixMul(Vector3Cross(Vector3Sub(Point,Position),Impulse),InvWorldInertiaTensor));
 end;
 PhysicsRigidBodyLimit(Instance);
end;

procedure PhysicsRigidBodyAddImpulse(var Instance:TPhysicsRigidBody;const Impulse:TPhysicsVector3); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
begin
 with Instance do begin
  Velocity:=Vector3Add(Velocity,Vector3ScalarMul(Impulse,InvMass));
 end;
 PhysicsRigidBodyLimit(Instance);
end;

procedure PhysicsRigidBodyDamp(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const dv=0.005;
var s:TPhysicsFloat;
begin
 with Instance do begin
  if abs(0.0-VelocityDamp)>EPSILON then begin
   Velocity:=Vector3ScalarMul(Velocity,power(1.0-VelocityDamp,TimeToWork));
  end;
  if abs(0.0-AngularVelocityDamp)>EPSILON then begin
   AngularVelocity:=Vector3ScalarMul(AngularVelocity,power(1.0-AngularVelocityDamp,TimeToWork));
  end;
  if AdditionalDamping then begin
   if (Vector3LengthSquared(Velocity)<VelocityAdditionalDampThresholdSqr) and
      (Vector3LengthSquared(AngularVelocity)<VelocityAdditionalDampThresholdSqr) then begin
    Velocity:=Vector3ScalarMul(Velocity,AdditionalDamp);
    AngularVelocity:=Vector3ScalarMul(AngularVelocity,AdditionalDamp);
   end;
   s:=Vector3Length(Velocity);
   if s<VelocityDamp then begin
    if s>dv then begin
     Velocity:=Vector3Sub(Velocity,Vector3ScalarMul(Vector3Norm(Velocity),dv));
    end else begin
     Velocity:=Vector3Origin;
    end;
   end;
   s:=Vector3Length(AngularVelocity);
   if s<AngularVelocityDamp then begin
    if s>dv then begin
     AngularVelocity:=Vector3Sub(AngularVelocity,Vector3ScalarMul(Vector3Norm(AngularVelocity),dv));
    end else begin
     AngularVelocity:=Vector3Origin;
    end;
   end;
  end;
 end;
end;

procedure PhysicsRigidBodyCalcForce(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  Force:=Vector3Origin;
  Torque:=Vector3Origin;
  if DoGravitation and not (Frozen or Static) then begin
   Force:=Vector3Add(Force,Vector3ScalarMul(PhysicsInstance^.Gravitation,Mass));
  end;
  Force:=Vector3Sub(Force,Vector3ScalarMul(Velocity,0.1));
 end;
end;

procedure PhysicsRigidBodyIntegrateVelocity(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  Velocity:=Vector3Add(Velocity,Vector3ScalarMul(Force,TimeToWork*InvMass));
  AngularVelocity:=Vector3Add(AngularVelocity,Vector3TermMatrixMul(Vector3ScalarMul(Torque,TimeToWork),InvWorldInertiaTensor));
 end;
 PhysicsRigidBodyLimit(Instance);
end;

procedure PhysicsRigidBodyIntegratePos(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  if Static then begin
   Velocity:=Vector3Origin;
   AngularVelocity:=Vector3Origin;
  end;
  
  Position:=Vector3Add(Position,Vector3ScalarMul(Velocity,TimeToWork));

  Orientation:=Matrix3x3TermAdd(Orientation,Matrix3x3TermScalarMul(Matrix3x3TermMul(Matrix3x3(0,-AngularVelocity.z,AngularVelocity.x,
                                                                                              AngularVelocity.z,0,-AngularVelocity.x,-AngularVelocity.y,
                                                                                              AngularVelocity.x,0),Orientation),TimeToWork));
  Matrix3x3OrthoNormalize(Orientation);

  InvWorldInertiaTensor:=Matrix3x3TermMul(Matrix3x3TermMul(Orientation,InvBodyInertiaTensor),Matrix3x3TermTranspose(Orientation));

  OldTransform:=Transform;
  OldInvTransform:=InvTransform;
  Transform:=Matrix4x4Set(Orientation);
  Transform[3,0]:=Position.x;
  Transform[3,1]:=Position.y;
  Transform[3,2]:=Position.z;
  Matrix4x4Inverse(InvTransform,Transform);

  OwnerObject^.OldTransform:=OwnerObject^.Transform;
  OwnerObject^.OldInvTransform:=OwnerObject^.InvTransform;
  OwnerObject^.Transform:=Transform;
  OwnerObject^.InvTransform:=InvTransform;

  PhysicsObjectUpdatePosition(OwnerObject^,Position);
 end;
end;

procedure PhysicsRigidBodyFindContacts(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 with Instance do begin
  OldVelocity:=Velocity;
  OldAngularVelocity:=AngularVelocity;
  OldPosition:=Position;
  OldOrientation:=Orientation;
  OldInvWorldInertiaTensor:=InvWorldInertiaTensor;

  PhysicsRigidBodyIntegrateVelocity(Instance,TimeToWork);
  PhysicsRigidBodyIntegratePos(Instance,TimeToWork);

  if assigned(OwnerObject) then begin
   if OwnerObject^.CollisionReceiver then begin
    PhysicsCollide(Collide^,OwnerObject);
   end;
  end;

  Velocity:=OldVelocity;
  AngularVelocity:=OldAngularVelocity;
  Position:=OldPosition;
  Orientation:=OldOrientation;
  InvWorldInertiaTensor:=OldInvWorldInertiaTensor;
 end;
end;

function PhysicsRigidBodyContactsResponse(var Instance:TPhysicsRigidBody;TimeToWork:TPhysicsFloat;ZeroRestitution:boolean=false):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i:integer;
    c:PPhysicsContact;
    RigidBody:PPhysicsRigidBody;
    r,r0,r1,Vel,Tangent,Impulse:TPhysicsVector3;
    NormalVel,ImpulseNumerator,ImpulseDenominator,TangentVel,FrictionNumerator,FrictionDenominator:TPhysicsFloat;
    Done:boolean;
begin
 with Instance do begin
  Done:=true;
  for i:=0 to Collide^.NumContacts-1 do begin
   c:=Collide^.Contacts^[i];
   if assigned(c^.ContactObject^.RigidBody) then begin
    RigidBody:=c^.ContactObject^.RigidBody;

    r0:=Vector3Sub(c^.Point,Position);
    r1:=Vector3Sub(c^.Point,RigidBody^.Position);

    Vel:=Vector3Sub(Vector3Add(Vector3Cross(AngularVelocity,r0),Velocity),Vector3Add(Vector3Cross(RigidBody^.AngularVelocity,r1),RigidBody^.Velocity));

    NormalVel:=Vector3Dot(c^.Normal,Vel);
    if NormalVel>(-EPSILON) then continue;

    if ZeroRestitution then begin
     ImpulseNumerator:=-NormalVel+(c^.Depth*(PhysicsInstance^.PenetrationSpeed/TimeToWork));
    end else begin
     ImpulseNumerator:=-((1+Restitution)*NormalVel);
    end;

    if ImpulseNumerator<EPSILON then continue;

    Tangent:=Vector3Neg(Vector3Sub(Vel,Vector3ScalarMul(c^.Normal,NormalVel)));

    Done:=false;

    ImpulseDenominator:=InvMass+
                        RigidBody^.InvMass+
                        Vector3Dot(c^.Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r0,c^.Normal),InvWorldInertiaTensor),r0))+
                        Vector3Dot(c^.Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r1,c^.Normal),RigidBody^.InvWorldInertiaTensor),r1));

    if ImpulseDenominator<EPSILON then begin
     result:=false;
     exit;
    end;

    Impulse:=Vector3ScalarMul(c^.Normal,ImpulseNumerator/ImpulseDenominator);

    if not (Frozen or Immovable or Static) then begin
     Velocity:=Vector3Add(Velocity,Vector3ScalarMul(Impulse,InvMass));
     AngularVelocity:=Vector3Add(AngularVelocity,Vector3TermMatrixMul(Vector3Cross(r0,Impulse),InvWorldInertiaTensor));
     PhysicsRigidBodyLimit(Instance);
    end;

    if not (RigidBody^.Frozen or RigidBody^.Immovable or RigidBody^.Static) then begin
     RigidBody^.Velocity:=Vector3Sub(RigidBody^.Velocity,Vector3ScalarMul(Impulse,RigidBody^.InvMass));
     RigidBody^.AngularVelocity:=Vector3Sub(RigidBody^.AngularVelocity,Vector3TermMatrixMul(Vector3Cross(r1,Impulse),RigidBody^.InvWorldInertiaTensor));
     PhysicsRigidBodyLimit(RigidBody^);
    end;

    if Vector3Length(Tangent)<EPSILON then continue;

    Tangent:=Vector3Norm(Tangent);

    Vel:=Vector3Sub(Vector3Add(Vector3Cross(AngularVelocity,r0),Velocity),Vector3Add(Vector3Cross(RigidBody^.AngularVelocity,r1),RigidBody^.Velocity));

    TangentVel:=Vector3Dot(Tangent,Vel);
    if TangentVel>-EPSILON then continue;

    FrictionNumerator:=-(TangentVel*Friction);

    FrictionDenominator:=InvMass+
                         RigidBody^.InvMass+
                         Vector3Dot(Tangent,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r0,Tangent),InvWorldInertiaTensor),r0))+
                         Vector3Dot(Tangent,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r1,Tangent),RigidBody^.InvWorldInertiaTensor),r1));

    Impulse:=Vector3ScalarMul(Tangent,FrictionNumerator/FrictionDenominator);

    if not Frozen then begin
     Velocity:=Vector3Add(Velocity,Vector3ScalarMul(Impulse,InvMass));
     AngularVelocity:=Vector3Add(AngularVelocity,Vector3TermMatrixMul(Vector3Cross(r0,Impulse),InvWorldInertiaTensor));
     PhysicsRigidBodyLimit(Instance);
    end;

    if not RigidBody^.Frozen then begin
     RigidBody^.Velocity:=Vector3Sub(RigidBody^.Velocity,Vector3ScalarMul(Impulse,RigidBody^.InvMass));
     RigidBody^.AngularVelocity:=Vector3Sub(RigidBody^.AngularVelocity,Vector3TermMatrixMul(Vector3Cross(r1,Impulse),RigidBody^.InvWorldInertiaTensor));
     PhysicsRigidBodyLimit(RigidBody^);
    end;
   end else begin
    if Frozen then continue;

    r:=Vector3Sub(c^.Point,Position);

    Vel:=Vector3Add(Vector3Cross(AngularVelocity,r),Velocity);

    NormalVel:=Vector3Dot(c^.Normal,Vel);
    if NormalVel>(-EPSILON) then continue;

    if ZeroRestitution then begin
     ImpulseNumerator:=-NormalVel+(c^.Depth*(PhysicsInstance^.PenetrationSpeed/TimeToWork));
    end else begin
     ImpulseNumerator:=-((1+Restitution)*NormalVel);
    end;

    if ImpulseNumerator<EPSILON then continue;

    Tangent:=Vector3Neg(Vector3Sub(Vel,Vector3ScalarMul(c^.Normal,NormalVel)));

    Done:=false;

    ImpulseDenominator:=InvMass+
                        Vector3Dot(c^.Normal,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r,c^.Normal),InvWorldInertiaTensor),r));

    if ImpulseDenominator<EPSILON then begin
     result:=false;
     exit;
    end;

    Impulse:=Vector3ScalarMul(c^.Normal,ImpulseNumerator/ImpulseDenominator);

    Velocity:=Vector3Add(Velocity,Vector3ScalarMul(Impulse,InvMass));
    AngularVelocity:=Vector3Add(AngularVelocity,Vector3TermMatrixMul(Vector3Cross(r,Impulse),InvWorldInertiaTensor));
    PhysicsRigidBodyLimit(Instance);

    if Vector3Length(Tangent)<EPSILON then continue;

    Tangent:=Vector3Norm(Tangent);

    Vel:=Vector3Add(Vector3Cross(AngularVelocity,r),Velocity);

    TangentVel:=Vector3Dot(Tangent,Vel);
    if TangentVel>(-EPSILON) then continue;

    FrictionNumerator:=-(TangentVel*Friction);

    FrictionDenominator:=InvMass+
                         Vector3Dot(Tangent,Vector3Cross(Vector3TermMatrixMul(Vector3Cross(r,Tangent),InvWorldInertiaTensor),r));

    Impulse:=Vector3ScalarMul(Tangent,FrictionNumerator/FrictionDenominator);

    Velocity:=Vector3Add(Velocity,Vector3ScalarMul(Impulse,InvMass));
    AngularVelocity:=Vector3Add(AngularVelocity,Vector3TermMatrixMul(Vector3Cross(r,Impulse),InvWorldInertiaTensor));
    PhysicsRigidBodyLimit(Instance);

   end;
  end;
  result:=Done;
 end;
end;

procedure PhysicsSweepAndPruneObjectAddPair(var Instance:TPhysics;ObjectA,ObjectB:PPhysicsObject;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
const FlagALLAXIS=(1 shl 0) or (1 shl 1) or (1 shl 2);
var Item:PPhysicsObjectSweepAndPruneCacheItem;
    Pair:PPhysicsObjectSweepAndPrunePair;
begin
 with ObjectA^ do begin
  if Instance.SweepAndPruneWorkMode=sapwmAXISALL then begin
   if ObjectB^.ID>=SweepAndPruneCacheSize then begin
    SweepAndPruneCacheSize:=(ObjectB^.ID+MemoryInc) and not MemoryIncMask;
    PhysicsReallocateMemory(SweepAndPruneCache,SweepAndPruneCacheSize*sizeof(integer));
   end;
   Item:=@SweepAndPruneCache^[ObjectB^.ID];
   if ((Item^>=0) and (Item^<SweepAndPrunePairsCount)) and (SweepAndPrunePairs^[Item^].WithObject=ObjectB) then begin
    Pair:=@SweepAndPrunePairs^[Item^];
    Pair^.Flags:=Pair^.Flags or (1 shl Axis);
    exit;
   end;
  end else begin
   Item:=nil;
  end;
  if SweepAndPrunePairsCount>=SweepAndPrunePairsSize then begin
   SweepAndPrunePairsSize:=(SweepAndPrunePairsCount+MemoryInc) and not MemoryIncMask;
   PhysicsReallocateMemory(SweepAndPrunePairs,SweepAndPrunePairsSize*sizeof(TPhysicsObjectSweepAndPrunePair));
  end;
  Pair:=@SweepAndPrunePairs^[SweepAndPrunePairsCount];
  Pair^.WithObject:=ObjectB;
  Pair^.Flags:=1 shl Axis;
  if assigned(Item) then begin
   Item^:=SweepAndPrunePairsCount;
  end;
  inc(SweepAndPrunePairsCount);
 end;
end;

procedure PhysicsSweepAndPruneAddPair(var Instance:TPhysics;ObjectA,ObjectB:PPhysicsObject;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsSweepAndPruneObjectAddPair(Instance,ObjectA,ObjectB,Axis);
 PhysicsSweepAndPruneObjectAddPair(Instance,ObjectB,ObjectA,Axis);
end;

procedure PhysicsSweepAndPruneAxisSort(var Instance:TPhysics;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var PartA,PartB,CurrentObject:PPhysicsObject;
    InSize,PartASize,PartBSize,Merges:integer;
begin
 if assigned(Instance.SweepAndPrune.Lists[Axis].First) then begin
  InSize:=1;
  while true do begin
   PartA:=Instance.SweepAndPrune.Lists[Axis].First;
   Instance.SweepAndPrune.Lists[Axis].First:=nil;
   Instance.SweepAndPrune.Lists[Axis].Last:=nil;
   Merges:=0;
   while assigned(PartA) do begin
    inc(Merges);
    PartB:=PartA;
    PartASize:=0;
    while PartASize<InSize do begin
     inc(PartASize);
     PartB:=PartB^.SweepAndPruneAllAxis[Axis].Next;
     if not assigned(PartB) then begin
      break;
     end;
    end;
    PartBSize:=InSize;
    while (PartASize>0) or ((PartBSize>0) and assigned(PartB)) do begin
     if PartASize=0 then begin
      CurrentObject:=PartB;
      PartB:=PartB^.SweepAndPruneAllAxis[Axis].Next;
      dec(PartBSize);
     end else if (PartBSize=0) or not assigned(PartB) then begin
      CurrentObject:=PartA;
      PartA:=PartA^.SweepAndPruneAllAxis[Axis].Next;
      dec(PartASize);
     end else if PartA^.TransformAABB.Min.xyz[Axis]<=PartB^.TransformAABB.Min.xyz[Axis] then begin
      CurrentObject:=PartA;
      PartA:=PartA^.SweepAndPruneAllAxis[Axis].Next;
      dec(PartASize);
     end else begin
      CurrentObject:=PartB;
      PartB:=PartB^.SweepAndPruneAllAxis[Axis].Next;
      dec(PartBSize);
     end;
     if assigned(Instance.SweepAndPrune.Lists[Axis].Last) then begin
      Instance.SweepAndPrune.Lists[Axis].Last^.SweepAndPruneAllAxis[Axis].Next:=CurrentObject;
     end else begin
      Instance.SweepAndPrune.Lists[Axis].First:=CurrentObject;
     end;
     CurrentObject^.SweepAndPruneAllAxis[Axis].Previous:=Instance.SweepAndPrune.Lists[Axis].Last;
     Instance.SweepAndPrune.Lists[Axis].Last:=CurrentObject;
    end;
    PartA:=PartB;
   end;
   Instance.SweepAndPrune.Lists[Axis].Last^.SweepAndPruneAllAxis[Axis].Next:=nil;
   if Merges<=1 then begin
    break;
   end;
   inc(InSize,InSize);
  end;
 end;
end;

procedure PhysicsSweepAndPruneAxisSearch(var Instance:TPhysics;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var CurrentObject,NextObject,CurrentNextObject:PPhysicsObject;
    sums:array[0..1] of TPhysicsVector3;
    v:TPhysicsVector3;
    n:integer;
    nf:TPhysicsFloat;
begin
 sums[0]:=Vector3Origin;
 sums[1]:=Vector3Origin;
 n:=0;
 CurrentObject:=Instance.SweepAndPrune.Lists[Axis].First;
 while assigned(CurrentObject) do begin
  NextObject:=CurrentObject^.SweepAndPruneAllAxis[Axis].Next;
  if PhysicsInstance^.SweepAndPruneWorkMode=sapwmAXISALL then begin
   CurrentNextObject:=NextObject;
   while assigned(CurrentNextObject) and not (CurrentNextObject^.TransformAABB.Min.xyz[Axis]>CurrentObject^.TransformAABB.Max.xyz[Axis]) do begin
    PhysicsSweepAndPruneAddPair(Instance,CurrentObject,CurrentNextObject,Axis);
    CurrentNextObject:=CurrentNextObject.SweepAndPruneAllAxis[Axis].Next;
   end;
  end else begin
   if PhysicsInstance^.SweepAndPruneWorkMode=sapwmAXISAUTO then begin
    sums[0]:=Vector3Add(sums[0],CurrentObject^.TransformSphere.Center);
    sums[1]:=Vector3Add(sums[1],Vector3Mul(CurrentObject^.TransformSphere.Center,CurrentObject^.TransformSphere.Center));
    inc(n);
   end;
   CurrentNextObject:=NextObject;
   while assigned(CurrentNextObject) and not (CurrentNextObject^.TransformAABB.Min.xyz[Axis]>CurrentObject^.TransformAABB.Max.xyz[Axis]) do begin
    if SphereIntersectEx(CurrentObject^.TransformSphere,CurrentNextObject^.TransformSphere) then begin
     PhysicsSweepAndPruneAddPair(Instance,CurrentObject,CurrentNextObject,Axis);
    end;
    CurrentNextObject:=CurrentNextObject.SweepAndPruneAllAxis[Axis].Next;
   end;
  end;
  CurrentObject:=NextObject;
 end;
 if PhysicsInstance^.SweepAndPruneWorkMode=sapwmAXISAUTO then begin
  if n=0 then begin
   PhysicsInstance^.SweepAndPruneAxis:=0;
  end else begin
   nf:=1/n;
   v:=Vector3ScalarMul(sums[0],nf);
   v:=Vector3Sub(Vector3ScalarMul(sums[1],nf),Vector3Mul(v,v));
   PhysicsInstance^.SweepAndPruneAxis:=0;
   if v.xyz[1]>v.xyz[0] then begin
    PhysicsInstance^.SweepAndPruneAxis:=1;
   end;
   if v.xyz[2]>v.xyz[PhysicsInstance^.SweepAndPruneAxis] then begin
    PhysicsInstance^.SweepAndPruneAxis:=2;
   end;
  end;
 end;
end;

procedure PhysicsSweepAndPruneAxisUpdate(var Instance:TPhysics;Axis:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 PhysicsSweepAndPruneAxisSort(Instance,Axis);
 PhysicsSweepAndPruneAxisSearch(Instance,Axis);
end;

procedure PhysicsSweepAndPruneUpdate(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var Axis:integer;
    AObject:PPhysicsObject;
begin
 AObject:=Instance.ObjectFirst;
 while assigned(AObject) do begin
  AObject^.SweepAndPrunePairsCount:=0;
  AObject:=AObject^.Next;
 end;
 case PhysicsInstance^.SweepAndPruneWorkMode of
  sapwmAXISALL:begin
   for Axis:=0 to 2 do begin
    PhysicsSweepAndPruneAxisUpdate(Instance,Axis);
   end;
  end;
  sapwmAXISX..sapwmAXISZ:begin
   PhysicsSweepAndPruneAxisUpdate(Instance,PhysicsInstance^.SweepAndPruneWorkMode);
  end;
  sapwmAXISAUTO:begin
   PhysicsSweepAndPruneAxisUpdate(Instance,PhysicsInstance^.SweepAndPruneAxis);
  end;
 end;
end;

procedure PhysicsInit(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin
 fillchar(Instance,sizeof(Instance),#0);
 with Instance do begin
  SweepAndPruneWorkMode:=sapwmAXISAUTO;
  SweepAndPruneAxis:=0;
  ConvexHullGenerationWorkMode:=chgwmBEROHULL;
  ConvexHullGenerationLevelOfDetail:=-1;
  ConvexHullGenerationPlaneSideCheckAtGeneration:=false;
  ConvexHullGenerationPlaneSideCheckAtTest:=true;
  Time:=0;
  TimeStep:=1/60;
  Gravitation:=Vector3(0,-9.8*0.5,0);
  VelocityMax:=20;
  VelocityThreshold:=sqr(0.1);
  AngularVelocityMax:=pi*0.5;
  AngularVelocityThreshold:=sqr(2*DEG2RAD);
  TimeToFrost:=1/10;
  PenetrationSpeed:=1/5;
  NumFirstIterations:=5;
  NumSecondIterations:=15;
  AllJoints:=0;
  NumJoints:=0;
  Joints:=nil;
  AllConstraints:=0;
  NumConstraints:=0;
  Constraints:=nil;
  AllRigidBodies:=0;
  NumRigidBodies:=0;
  RigidBodies:=nil;
  ObjectFirst:=nil;
  ObjectLast:=nil;
  ObjectID:=0;
 end;
end;

procedure PhysicsDone(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
begin              
 with Instance do begin
  if assigned(Joints) then begin
   freemem(Joints);
   Joints:=nil;
  end;
  if assigned(Constraints) then begin
   freemem(Constraints);
   Constraints:=nil;
  end;
  if assigned(RigidBodies) then begin
   freemem(RigidBodies);
   RigidBodies:=nil;
  end;
 end;
 fillchar(Instance,sizeof(Instance),#0);
end;

function PhysicsRigidBodyCompare(const a,b:pointer):integer; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var DA:PPhysicsRigidBody absolute a;
    DB:PPhysicsRigidBody absolute b;
begin
 if DA^.Position.y>DB^.Position.y then begin
  result:=-1;
 end else if DA^.Position.y<DB^.Position.y then begin
  result:=1;
 end else begin
  result:=0;
 end;
end;

procedure PhysicsUpdateOldObjectValues(var Instance:TPhysics); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
 procedure ProcessObject(ObjectToCheck:PPhysicsObject);
 begin
  if (not assigned(ObjectToCheck^.RigidBody)) or (assigned(ObjectToCheck^.RigidBody) and (ObjectToCheck^.RigidBody^.Frozen or ObjectToCheck^.RigidBody^.Immovable or ObjectToCheck^.RigidBody^.Static)) then begin
   if ObjectToCheck^.HaveNewTransform then begin
    ObjectToCheck^.OldPosition:=ObjectToCheck^.Position;
    ObjectToCheck^.OldTransform:=ObjectToCheck^.Transform;
    ObjectToCheck^.OldInvTransform:=ObjectToCheck^.InvTransform;
    ObjectToCheck^.HaveNewTransform:=false;
   end;
  end;
 end;
var CurObj:PPhysicsObject;
begin
 CurObj:=Instance.ObjectFirst;
 while assigned(CurObj) do begin
  ProcessObject(CurObj);
  CurObj:=CurObj^.Next;
 end;
end;

procedure PhysicsUpdate(var Instance:TPhysics;TimeToWork:TPhysicsFloat); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var i,j:integer;
    CurObj:PPhysicsObject;
    RigidBody:PPhysicsRigidBody;
    Done:boolean;
begin
 with Instance do begin
  //PhysicsSort(RigidBodies,NumRigidBodies,PhysicsRigidBodyCompare);

  Time:=Time+TimeToWork;
  while Time>TimeStep do begin
   Time:=Time-TimeStep;

   CurObj:=Instance.ObjectFirst;
   while assigned(CurObj) do begin
    CurObj^.Time:=CurObj^.Time+TimeStep;
    CurObj:=CurObj^.Next;
   end;

   PhysicsSort(RigidBodies,NumRigidBodies,PhysicsRigidBodyCompare);
   PhysicsSweepAndPruneUpdate(Instance);
   PhysicsUpdateOldObjectValues(Instance);

   for i:=0 to NumRigidBodies-1 do begin
    RigidBody:=RigidBodies^[i];
    with RigidBody^ do begin
     Force:=Vector3Origin;
     Torque:=Vector3Origin;
    end;
    if not RigidBody^.Frozen then begin
     PhysicsRigidBodyFindContacts(RigidBody^,TimeStep);
    end;
    RigidBody^.Frozen:=false;
   end;

   for i:=1 to NumFirstIterations do begin
    Done:=true;
    for j:=0 to NumRigidBodies-1 do begin
     if not PhysicsRigidBodyContactsResponse(RigidBodies^[j]^,TimeStep,false) then begin
      Done:=false;
     end;
    end;
    for j:=0 to NumJoints-1 do begin
     PhysicsJointResponse(Joints^[j]^,TimeStep);
    end;
    for j:=0 to NumConstraints-1 do begin
     PhysicsConstraintResponse(Constraints^[j]^,TimeStep);
    end;
    if Done then break;
   end;

   PhysicsSort(RigidBodies,NumRigidBodies,PhysicsRigidBodyCompare);
   PhysicsSweepAndPruneUpdate(Instance);
   PhysicsUpdateOldObjectValues(Instance);

   for i:=0 to NumRigidBodies-1 do begin
    RigidBody:=RigidBodies^[i];
    if (RigidBody^.Collide^.NumContacts>0) and (RigidBody^.NumJoints=0) and (Vector3LengthSquared(RigidBody^.Velocity)<VelocityThreshold) and (Vector3LengthSquared(RigidBody^.AngularVelocity)<AngularVelocityThreshold) then begin
     RigidBody^.FrozenTime:=RigidBody^.FrozenTime+TimeStep;
     if RigidBody^.FrozenTime>TimeToFrost then begin
      if RigidBody^.FrozenNumObjects=RigidBody^.Collide^.NumObjects then begin
       RigidBody^.Frozen:=true;
       RigidBody^.Velocity:=Vector3Origin;
       RigidBody^.AngularVelocity:=Vector3Origin;
      end else begin
       RigidBody^.Frozen:=false;
       RigidBody^.FrozenTime:=0;
       PhysicsRigidBodyFindContacts(RigidBody^,TimeStep);
      end;
     end;
     RigidBody^.FrozenNumObjects:=RigidBody^.Collide^.NumObjects;
    end else begin
     RigidBody^.Frozen:=false;
     RigidBody^.FrozenTime:=0;
     PhysicsRigidBodyFindContacts(RigidBody^,TimeStep);
    end;
    PhysicsRigidBodyDamp(RigidBody^,TimeStep);
    PhysicsRigidBodyCalcForce(RigidBody^,TimeStep);
    PhysicsRigidBodyIntegrateVelocity(RigidBody^,TimeStep);
   end;

   for i:=1 to NumSecondIterations do begin
    Done:=true;
    for j:=0 to NumRigidBodies-1 do begin
     if not PhysicsRigidBodyContactsResponse(RigidBodies^[j]^,TimeStep,true) then begin
      Done:=false;
     end;
    end;
    for j:=0 to NumJoints-1 do begin
     PhysicsJointResponse(Joints^[j]^,TimeStep);
    end;
    for j:=0 to NumConstraints-1 do begin
     PhysicsConstraintResponse(Constraints^[j]^,TimeStep);
    end;
    if Done then break;
   end;

   for i:=0 to NumRigidBodies-1 do begin
    RigidBody:=RigidBodies^[i];
    if not RigidBody^.Frozen then begin
     PhysicsRigidBodyIntegratePos(RigidBody^,TimeStep);
    end;
   end;
  end;

  for i:=0 to NumRigidBodies-1 do begin
   RigidBody:=RigidBodies^[i];
   with RigidBody^ do begin
    Immovable:=false;
   end;
  end;

 end;
end;

function PhysicsRayIntersection(var Instance:TPhysics;const Origin,Direction:TPhysicsVector3;var Point,Normal:TPhysicsVector3;UserRayProc:TPhysicsObjectRayIntersectionUserProc=nil;UserData:pointer=nil):PPhysicsObject; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif}
var BestObj:PPhysicsObject;
    BestPoint,BestNormal:TPhysicsVector3;
    BestDist:TPhysicsFloat;
 procedure ProcessObject(CurObj:PPhysicsObject);
 var CurPoint,CurNormal:TPhysicsVector3;
     CurDist:TPhysicsFloat;
 begin
  if not assigned(CurObj) then exit;
  if not CurObj^.CollisionSender then exit;
  if PhysicsObjectRayIntersection(CurObj^,Origin,Direction,CurPoint,CurNormal,UserRayProc,UserData) then begin
   CurDist:=Vector3Dist(Origin,CurPoint);
   if (CurDist<BestDist) or not assigned(BestObj) then begin
    BestDist:=CurDist;
    BestObj:=CurObj;
    BestPoint:=CurPoint;
    BestNormal:=CurNormal;
   end;
  end;
 end;
var CurObj:PPhysicsObject;
begin
 BestObj:=nil;
 BestPoint:=Vector3Origin;
 BestNormal:=Vector3Origin;
 BestDist:=0;
 CurObj:=Instance.ObjectFirst;
 while assigned(CurObj) do begin
  ProcessObject(CurObj);
  CurObj:=CurObj^.Next;
 end;
 result:=BestObj;
 if assigned(result) then begin
  Point:=BestPoint;
  Normal:=BestNormal;
 end;
end;

procedure ReallocateMemory(var p;Size:integer); {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} // For buggy system units with buggy reallocmem implementations
begin
 if assigned(pointer(p)) then begin
  if Size=0 then begin
   freemem(pointer(p));
   pointer(p):=nil;
  end else begin
   reallocmem(pointer(p),Size);
  end;
 end else if Size<>0 then begin
  getmem(pointer(p),Size);
 end;
end;

function PhysicsObjectLoadMesh(var DstObject:TPhysicsObject;SrcData:pointer;SrcSize:longword):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} {$ifdef win32}overload;{$endif}
const CHUNK_3DS_MAIN=$4d4d;
      CHUNK_3DS_OBJMESH=$3d3d;
      CHUNK_3DS_OBJBLOCK=$4000;
      CHUNK_3DS_TRIMESH=$4100;
      CHUNK_3DS_VERTLIST=$4110;
      CHUNK_3DS_FACELIST=$4120;
      CHUNK_3DS_MAPLIST=$4140;
      CHUNK_3DS_SMOOTHLIST=$4150;
      CHUNK_3DS_MESHMATRIX=$4160;
type TFileSignature=array[0..3] of char;
     PPhysicsVector2Array=^TPhysicsVector2Array;
     TPhysicsVector2Array=array[0..0] of TPhysicsVector2;
     PPhysicsVector3Array=^TPhysicsVector3Array;
     TPhysicsVector3Array=array[0..0] of TPhysicsVector3;
     PFace3DS=^TFace3DS;

     TFace3DS=record
      Indices:array[0..2] of longword;
      Flags:longword;
      SmoothGroup:longword;
     end;
     PFaces3DS=^TFaces3DS;
     TFaces3DS=array[0..0] of TFace3DS;
     PObject3DSMesh=^TObject3DSMesh;

     TObject3DSMesh=record
      Vertices:PPhysicsVector3Array;
      NumVertices:integer;
      TexCoords:PPhysicsVector2Array;
      NumTexCoords:integer;
      Faces:PFaces3DS;
      NumFaces:integer;
      Matrix:TPhysicsMatrix4x4;
     end;
     PObject3DSMeshs=^TObject3DSMeshs;
     TObject3DSMeshs=array[0..0] of TObject3DSMesh;
     PObject3DS=^TObject3DS;

     TObject3DS=record
      name:pchar;
      Meshs:PObject3DSMeshs;
      NumMeshs:integer;
     end;
     PObjects3DS=^TObjects3DS;
     TObjects3DS=array[0..0] of TObject3DS;
var SrcPos:longword;
    Signature:TFileSignature;
    Signature3DS:word;
    Size3DS:longword;
    Objects3DS:PObjects3DS;
    NumObjects3DS:integer;
 function read(var Dst;DstLen:longword):longword;
 begin
  result:=SrcSize-SrcPos;
  if result>DstLen then result:=DstLen;
  if result=0 then exit;
  move(pchar(SrcData)[SrcPos],Dst,result);
  inc(SrcPos,result);
 end;
 function ReadByte:byte;
 begin
  read(result,sizeof(byte));
 end;
 function ReadWord:word;
 begin
  read(result,sizeof(word));
 end;
 function ReadLongWord:longword;
 begin
  read(result,sizeof(longword));
 end;
 function ReadFloat:single;
 begin
  read(result,sizeof(single));
 end;
 function Read3DSChunks(const ParentChunk,Bytes:longword):longword;forward;
 function Skip3DSString:longword;
 var c:char;
 begin
  result:=0;
  c:=#255;
  while c<>#0 do begin
   if read(c,sizeof(char))<>sizeof(char) then begin
    break;
   end;
   inc(result);
  end;
 end;
 function Read3DSString(var p:pchar):longword;
 var c:char;
     OldPos:longword;
 begin
  OldPos:=SrcPos;
  result:=0;
  c:=#255;
  while c<>#0 do begin
   if read(c,sizeof(char))<>sizeof(char) then begin
    break;
   end;
   inc(result);
  end;
  getmem(p,result);
  SrcPos:=OldPos;
  result:=0;
  c:=#255;
  while c<>#0 do begin
   if read(c,sizeof(char))<>sizeof(char) then begin
    break;
   end;
   p[result]:=c;
   inc(result);
  end;
 end;
 function Read3DSChunk(const ParentChunk:longword):longword;
 var Chunk:word;
     Size,I,J:longword;
     Vertex:PPhysicsVector3;
     TexCoord:PPhysicsVector2;
     Face:PFace3DS;
 begin
  if read(Chunk,sizeof(word))<>sizeof(word) then begin
   result:=$80000000;
   exit;
  end;
  if read(result,sizeof(longword))<>sizeof(longword) then begin
   result:=$80000000;
   exit;
  end;
  Size:=result-6;
  case ParentChunk of
   CHUNK_3DS_MAIN:begin
    case Chunk of
     CHUNK_3DS_OBJMESH:begin
      Read3DSChunks(Chunk,Size);
     end;
     else begin
      inc(SrcPos,Size);
     end;
    end;
   end;
   CHUNK_3DS_OBJMESH:begin
    case Chunk of
     CHUNK_3DS_OBJBLOCK:begin
      inc(NumObjects3DS);
      ReallocateMemory(Objects3DS,NumObjects3DS*sizeof(TObject3DS));
      fillchar(Objects3DS^[NumObjects3DS-1],sizeof(TObject3DS),#0);
      dec(Size,Read3DSString(Objects3DS^[NumObjects3DS-1].name));
      Read3DSChunks(Chunk,Size);
     end;
     else begin
      inc(SrcPos,Size);
     end;
    end;
   end;
   CHUNK_3DS_OBJBLOCK:begin
    case Chunk of
     CHUNK_3DS_TRIMESH:begin
      inc(Objects3DS^[NumObjects3DS-1].NumMeshs);
      ReallocateMemory(Objects3DS^[NumObjects3DS-1].Meshs,Objects3DS^[NumObjects3DS-1].NumMeshs*sizeof(TObject3DSMesh));
      fillchar(Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1],sizeof(TObject3DSMesh),#0);
      Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Matrix:=Matrix4x4Identity;
      Read3DSChunks(Chunk,Size);
     end;
     else begin
      inc(SrcPos,Size);
     end;
    end;
   end;
   CHUNK_3DS_TRIMESH:begin
    case Chunk of
     CHUNK_3DS_VERTLIST:begin
      Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumVertices:=ReadWord;
      ReallocateMemory(Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Vertices,Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumVertices*sizeof(TPhysicsVector3));
      Vertex:=@Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Vertices^[0];
      for I:=1 to Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumVertices do begin
       Vertex^.X:=ReadFloat;
       Vertex^.Y:=ReadFloat;
       Vertex^.Z:=ReadFloat;
       inc(Vertex);
      end;
     end;
     CHUNK_3DS_MAPLIST:begin
      Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumTexCoords:=ReadWord;
      ReallocateMemory(Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].TexCoords,Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumTexCoords*sizeof(TPhysicsVector2));
      TexCoord:=@Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].TexCoords^[0];
      for I:=1 to Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumTexCoords do begin
       TexCoord^.X:=ReadFloat;
       TexCoord^.Y:=ReadFloat;
       inc(TexCoord);
      end;
     end;
     CHUNK_3DS_FACELIST:begin
      Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumFaces:=ReadWord;
      ReallocateMemory(Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Faces,Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumFaces*sizeof(TFace3DS));
      Face:=@Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Faces^[0];
      for I:=1 to Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumFaces do begin
       Face^.Indices[0]:=ReadWord;
       Face^.Indices[1]:=ReadWord;
       Face^.Indices[2]:=ReadWord;
       Face^.Flags:=ReadWord;
       inc(Face);
      end;
      dec(Size,(Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumFaces*4)+2);
      Read3DSChunks(Chunk,Size);
     end;
     CHUNK_3DS_MESHMATRIX:begin
      Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Matrix:=Matrix4x4Identity;
      for I:=0 to 3 do begin
       for J:=0 to 2 do begin
        Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Matrix[I,J]:=ReadFloat;
       end;
      end;
     end;
     else begin
      inc(SrcPos,Size);
     end;
    end;
   end;
   CHUNK_3DS_FACELIST:begin
    case Chunk of
     CHUNK_3DS_SMOOTHLIST:begin
      Face:=@Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].Faces^[0];
      for I:=1 to Objects3DS^[NumObjects3DS-1].Meshs^[Objects3DS^[NumObjects3DS-1].NumMeshs-1].NumFaces do begin
       Face^.SmoothGroup:=ReadLongWord;
       inc(Face);
      end;
     end;
     else begin
      inc(SrcPos,Size);
     end;
    end;
   end;
   else begin
    inc(SrcPos,Size);
   end;
  end;
 end;
 function Read3DSChunks(const ParentChunk,Bytes:longword):longword;
 begin
  result:=0;
  while result<Bytes do begin
   inc(result,Read3DSChunk(ParentChunk));
  end;
 end;
 procedure Convert3DS;
 var I,J,K,H,M:integer;
     V:array[0..2] of TPhysicsVector3;
     TV:TPhysicsVector3;
 begin
  for I:=0 to NumObjects3DS-1 do begin
   for J:=0 to Objects3DS^[I].NumMeshs-1 do begin
    M:=PhysicsObjectAddMesh(DstObject);
    for K:=0 to Objects3DS^[I].Meshs^[J].NumFaces-1 do begin
     for H:=0 to 2 do begin
      TV:=Objects3DS^[I].Meshs^[J].Vertices^[Objects3DS^[I].Meshs^[J].Faces^[K].Indices[H]];
      //Vector3MatrixMul(TV,Objects3DS^[I].Meshs^[J].Matrix);
      V[H].X:=TV.X;
      V[H].Y:=TV.Z;
      V[H].Z:=-TV.Y;
     end;
     PhysicsObjectMeshAddTriangle(DstObject.Meshs^[M]^,V[0],V[1],V[2]);
    end;
   end;
  end;
 end;
 procedure Free3DS;
 var I,J:integer;
 begin
  for I:=0 to NumObjects3DS-1 do begin
   for J:=0 to Objects3DS^[I].NumMeshs-1 do begin
    ReallocateMemory(Objects3DS^[I].Meshs^[J].Vertices,0);
    ReallocateMemory(Objects3DS^[I].Meshs^[J].TexCoords,0);
    ReallocateMemory(Objects3DS^[I].Meshs^[J].Faces,0);
   end;
   ReallocateMemory(Objects3DS^[I].Meshs,0);
  end;
  ReallocateMemory(Objects3DS,0);
 end;
 procedure LoadPMF;
 type TFace=record
       Indices:array[0..2] of longword;
      end;
      PFaces=^TFaces;
      TFaces=array[0..0] of TFace;
 var NumVertices,NumFaces:longword;
     Counter,M:integer;
     Faces:PFaces;
     Vertices:PPhysicsVector3Array;
 begin
  NumFaces:=ReadLongWord;
  NumVertices:=ReadLongWord;
  if (NumFaces=0) or (NumVertices=0) then exit;
  getmem(Faces,NumFaces*sizeof(TFace));
  for Counter:=0 to NumFaces-1 do begin
   Faces[Counter].Indices[0]:=ReadLongWord;
   Faces[Counter].Indices[1]:=ReadLongWord;
   Faces[Counter].Indices[2]:=ReadLongWord;
  end;
  getmem(Vertices,NumVertices*sizeof(TPhysicsVector3));
  for Counter:=0 to NumVertices-1 do begin
   Vertices[Counter].X:=ReadFloat;
   Vertices[Counter].Y:=ReadFloat;
   Vertices[Counter].Z:=ReadFloat;
  end;
  M:=PhysicsObjectAddMesh(DstObject);
  for Counter:=0 to NumFaces-1 do begin
   PhysicsObjectMeshAddTriangle(DstObject.Meshs^[M]^,Vertices[Faces[Counter].Indices[0]],Vertices[Faces[Counter].Indices[1]],Vertices[Faces[Counter].Indices[2]]);
  end;
  freemem(Vertices);
  freemem(Faces);
 end;
begin
 result:=false;
 if SrcSize=0 then exit;
 SrcPos:=0;
 if read(Signature,sizeof(TFileSignature))<>sizeof(TFileSignature) then exit;
 if (Signature[0]='P') and (Signature[1]='M') and (Signature[2]='F') and (Signature[3]='0') then begin
  LoadPMF;
 end else begin
  SrcPos:=0;
  if read(Signature3DS,sizeof(word))<>sizeof(word) then exit;
  if Signature3DS=CHUNK_3DS_MAIN then begin
   if read(Size3DS,sizeof(longword))<>sizeof(longword) then exit;
   Objects3DS:=nil;
   NumObjects3DS:=0;
   result:=Read3DSChunks(Signature3DS,Size3DS)>0;
   if assigned(Objects3DS) then begin
    if result then begin
     Convert3DS;
    end;
    Free3DS;
   end;
  end;
 end;
end;

{$ifdef win32}
function PhysicsObjectLoadMesh(var DstObject:TPhysicsObject;SrcFileName:pchar):boolean; {$ifdef physicsstdcall}stdcall;{$else}{$ifdef physicscdecl}cdecl;{$else}{$ifdef physicsregister}register;{$endif}{$endif}{$endif} overload;
var SrcFile:THANDLE;
    SrcSize,TempVar:longword;
    SrcData:pointer;
begin
 result:=false;
 SrcFile:=CreateFile(SrcFileName,GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,0,0);
 if SrcFile=INVALID_HANDLE_VALUE then begin
  CloseHandle(SrcFile);
  exit;
 end;
 SrcSize:=SetFilePointer(SrcFile,0,nil,FILE_END);
 if SrcSize=0 then begin
  CloseHandle(SrcFile);
  exit;
 end;
 SetFilePointer(SrcFile,0,nil,FILE_BEGIN);
 getmem(SrcData,SrcSize);
 ReadFile(SrcFile,SrcData^,SrcSize,TempVar,nil);
 CloseHandle(SrcFile);
 result:=PhysicsObjectLoadMesh(DstObject,SrcData,SrcSize);
 freemem(SrcData);
end;

{$endif}

end.
