# RenderParticles {#RenderParticles}

Render 2D-particles using the current material.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

Example usage: "CursorModel.OnRender" in @ref Particles sample project.

## Properties

@dl

@dt ParticlesPerSecond
@dd Number of particles to emit per second.

@dt Direction
@dd The center direction that particles move, in radians.

@dt Spread
@dd Variance in direction of particles, in radians. To fire particles in all direction set Direction=0 and Spread=3.14.

@dt ParticleWidth
@dd Width of particles.

@dt ParticleHeight
@dd Height of particles.

@dt Speed
@dd Base speed of particles.

@dt SpeedRange
@dd Variance in speed of particles.

@dt Radius
@dd Size of radius around current render position where particles are emitted.

@dt ParticleLifetime
@dd Lifetime in seconds of a particle.

@dt AnimateAlpha
@dd This value is added to the color-alpha value of each particle every second. Useful for making particles fade in or out over time.

@dt AnimateSize
@dd Dynamic changing of particle's size. If &gt;0, the particle size is increased. If &lt;0, the particle size is decreased. If 0, the particle does not change its size. Initial size is given by ParticleWidth and ParticleHeight properties.

@dt Damping
@dd Dynamic changing of particle's moving speed. If &gt;0 the speed is decreased. If &lt;0, the speed is increased.

@dt Duration
@dd The duration in seconds that new particles will be created. Set to 0 for no limit.

@dt BeginTime
@dd Delay in seconds before any particles will be created.

@dt OnEmitExpression
@dd An expression that will be called when a new particle is created. The following properties can be modified in the expression: PColor (particle color) and PAngle (particle angle).

@dt Gravity
@dd A gravitation speed that is added to every particles velocity each frame. Note that even though a Z-value can be set, only the X and Y values are used.

@dt FollowModel
@dd If set then the particles positions are relative to the current Model.

@dlx
